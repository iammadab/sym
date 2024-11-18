use crate::Expression;

pub(crate) fn simplify_add(expression: Expression) -> Expression {
    // first we simplify each term in the add expression before apply addition simplification
    let terms = expression.children().into_iter().map(|c| c.simplify());

    // if the addition expression contains another addition expression within, we collapse this
    // i.e. (a + b) + c = a + b + c
    let terms = terms.flat_map(|child| match child {
        Expression::Add(_) => child.children(),
        _ => vec![child],
    });

    // collect the constant integer terms
    // i.e 1 + 2 + 3 = 6
    let mut sum = 0;
    let terms = terms
        .filter(|t| match t {
            _ => true,
        })
        .collect::<Vec<_>>();

    // collect variable terms
    // x + y + 2x + z = 3x + y + z
    let mut variable_map = vec![];
    for term in terms {
        let terms_count_expr_breakdown = coefficient_expression_split(term);
        search_and_update_count(
            &mut variable_map,
            terms_count_expr_breakdown.0,
            terms_count_expr_breakdown.1,
        );
    }

    // convert the variable map back into terms
    let mut variable_map_rewrite_terms = vec![];
    for (count, expr) in variable_map {
        if count == Expression::integer(0) {
            continue;
        }

        if count == Expression::integer(1) {
            variable_map_rewrite_terms.push(expr);
            continue;
        }

        if count == Expression::integer(-1) {
            variable_map_rewrite_terms.push(Expression::Neg(Box::new(expr)));
            continue;
        }

        variable_map_rewrite_terms.push(Expression::Mul(vec![count, expr]));
    }

    let mut final_terms = variable_map_rewrite_terms;
    if sum != 0 {
        final_terms.push(Expression::integer(sum));
    }

    if final_terms.len() == 1 {
        return final_terms.pop().unwrap();
    }

    Expression::Add(final_terms)
}

fn coefficient_expression_split(expr: Expression) -> (Expression, Expression) {
    // it is assumed that the expression has already been simplified
    match expr {
        Expression::Neg(inner) => (Expression::integer(-1), *inner.clone()),
        Expression::Mul(ref exprs) => {
            // since this should already be simplified
            // we assume that there should be only one fractional expression
            // if none exists then the count is 1
            let mut terms = exprs.clone();

            let mut count = Expression::integer(1);
            if let Some(pos) = terms
                .iter()
                .position(|expr| matches!(expr, Expression::Fraction(..)))
            {
                count = terms.remove(pos);
            }

            if terms.len() == 1 {
                (count, terms.pop().unwrap())
            } else {
                (count, Expression::Mul(terms))
            }
        }
        _ => (Expression::integer(1), expr),
    }
}

// TODO: handle duplicate
fn search_and_update_count(
    store: &mut Vec<(Expression, Expression)>,
    count: Expression,
    expr: Expression,
) {
    for (prev_count, matching_expr) in &mut *store {
        if expr == *matching_expr {
            *prev_count = prev_count.clone() + count;
            return;
        }
    }

    store.push((count, expr));
}

fn sum_fraction(a: Expression, b: Expression) -> Expression {
    // assumes that a and b are factions
    // m/n + q/p = (mp + qn) / np
    let (a_num, a_denom) = a.decompose_fraction().unwrap();
    let (b_num, b_denom) = b.decompose_fraction().unwrap();

    Expression::Fraction((a_num * b_denom) + (b_num * a_denom), a_denom * b_denom).simplify()
}

#[cfg(test)]
mod tests {
    use crate::simplify::add::{
        coefficient_expression_split, search_and_update_count, sum_fraction,
    };
    use crate::Expression;

    #[test]
    fn test_add_simplification() {
        // integer collection
        assert_eq!(
            Expression::integer(1) + Expression::integer(2),
            Expression::integer(3)
        );

        // integer collection, mixed with variables
        assert_eq!(
            (Expression::integer(1)
                + Expression::Variable("x".to_string())
                + Expression::Variable("y".to_string())
                + Expression::integer(2))
            .to_string(),
            "(x + y + 3)"
        );

        // integer + single variable collection
        assert_eq!(
            // 1 + 2 + 2x + y + x + x
            // 4x + y + 3
            (Expression::integer(1)
                + Expression::integer(2)
                + Expression::Mul(vec![
                    Expression::Variable("x".to_string()),
                    Expression::integer(2)
                ])
                + Expression::Variable("y".to_string())
                + Expression::Variable("x".to_string())
                + Expression::Variable("x".to_string()))
            .simplify()
            .to_string(),
            "(4x + y + 3)"
        );

        // integer + multi-variable collection
        assert_eq!(
            // 1 + 2 + 2xy + 3yx + xy
            // 6xy + 3
            (Expression::integer(1)
                + Expression::integer(2)
                + Expression::Mul(vec![
                    Expression::integer(2),
                    Expression::Variable("x".to_string()),
                    Expression::Variable("y".to_string())
                ])
                + Expression::Mul(vec![
                    Expression::Variable("x".to_string()),
                    Expression::Variable("y".to_string())
                ])
                + Expression::Mul(vec![
                    Expression::Variable("y".to_string()),
                    Expression::integer(3),
                    Expression::Variable("x".to_string())
                ]))
            .simplify()
            .to_string(),
            "(6xy + 3)"
        );
    }

    #[test]
    fn test_coefficient_split() {
        assert_eq!(
            coefficient_expression_split(Expression::Variable("x".to_string())),
            (
                Expression::integer(1),
                Expression::Variable("x".to_string())
            )
        );
        assert_eq!(
            coefficient_expression_split(Expression::Mul(vec![
                Expression::Variable("x".to_string()),
                Expression::Variable("y".to_string())
            ])),
            (
                Expression::integer(1),
                Expression::Mul(vec![
                    Expression::Variable("y".to_string()),
                    Expression::Variable("x".to_string())
                ])
            )
        );
        assert_eq!(
            coefficient_expression_split(Expression::Mul(vec![
                Expression::Variable("x".to_string()),
                Expression::integer(3),
                Expression::Variable("y".to_string())
            ])),
            (
                Expression::integer(3),
                Expression::Mul(vec![
                    Expression::Variable("y".to_string()),
                    Expression::Variable("x".to_string())
                ])
            )
        );
        assert_eq!(
            coefficient_expression_split(Expression::Mul(vec![
                Expression::Variable("x".to_string()),
                Expression::Fraction(3, 2),
                Expression::Variable("y".to_string())
            ])),
            (
                Expression::Fraction(3, 2),
                Expression::Mul(vec![
                    Expression::Variable("y".to_string()),
                    Expression::Variable("x".to_string())
                ])
            )
        );
    }

    #[test]
    fn test_search_and_update() {
        let mut result = vec![];
        search_and_update_count(&mut result, Expression::integer(2), Expression::integer(3));
        assert_eq!(result.len(), 1);
        search_and_update_count(
            &mut result,
            Expression::integer(-1),
            Expression::Add(vec![
                Expression::integer(2),
                Expression::Variable("x".to_string()),
            ]),
        );
        assert_eq!(result.len(), 2);
        search_and_update_count(
            &mut result,
            Expression::integer(1),
            Expression::Add(vec![
                Expression::Variable("x".to_string()),
                Expression::integer(2),
            ]),
        );
        assert_eq!(result.len(), 2);
        assert_eq!(result[1].0, Expression::integer(0));
    }

    #[test]
    fn test_sum_fraction() {
        // 0 - 1 = -1
        assert_eq!(
            sum_fraction(
                Expression::integer(0),
                Expression::Neg(Box::new(Expression::integer(1))).simplify()
            ),
            Expression::integer(-1)
        );

        // 1/2 + 1/2 = 1
        assert_eq!(
            sum_fraction(Expression::Fraction(1, 2), Expression::Fraction(1, 2)),
            Expression::integer(1)
        );

        // 1/8 + 2/3 = 19/24
        assert_eq!(
            sum_fraction(Expression::Fraction(1, 8), Expression::Fraction(2, 3)),
            Expression::Fraction(19, 24)
        );
    }
}
