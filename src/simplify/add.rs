use crate::Expression;

pub(crate) fn simplify_add(expression: Expression) -> Expression {
    dbg!(&expression);
    dbg!("I was called");
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
            Expression::Integer(val) => {
                sum += val;
                false
            }
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
    dbg!(&variable_map);

    // convert the variable map back into terms
    let mut variable_map_rewrite_terms = vec![];
    for (count, expr) in variable_map {
        if count == 0 {
            continue;
        }

        if count == 1 {
            variable_map_rewrite_terms.push(expr);
            continue;
        }

        if count == -1 {
            // TODO: do I need to simplify here?
            variable_map_rewrite_terms.push(Expression::Neg(Box::new(expr)));
            continue;
        }

        // TODO: do I need to simplify here?
        variable_map_rewrite_terms.push(Expression::Mul(vec![Expression::Integer(count), expr]));
    }

    let mut final_terms = variable_map_rewrite_terms;
    if sum != 0 {
        final_terms.push(Expression::Integer(sum));
    }

    if final_terms.len() == 1 {
        return final_terms.pop().unwrap();
    }

    Expression::Add(final_terms)
}

fn coefficient_expression_split(expr: Expression) -> (isize, Expression) {
    // it is assumed that the expression has already been simplified
    match expr {
        Expression::Neg(inner) => (-1, *inner.clone()),
        Expression::Mul(ref exprs) => {
            // since this should already be simplified
            // we assume that there should be only one integer expression
            // if none exists then the count is 1
            let mut terms = exprs.clone();

            let mut count = 1;
            if let Some(pos) = terms
                .iter()
                .position(|expr| matches!(expr, Expression::Integer(_)))
            {
                if let Expression::Integer(val) = terms[pos] {
                    count = val;
                }
                terms.remove(pos);
            }

            if terms.len() == 1 {
                (count, terms.pop().unwrap())
            } else {
                (count, Expression::Mul(terms))
            }
        }
        _ => (1, expr),
    }
}

fn search_and_update_count(store: &mut Vec<(isize, Expression)>, count: isize, expr: Expression) {
    for (prev_count, matching_expr) in &mut *store {
        if expr == *matching_expr {
            *prev_count += count;
            return;
        }
    }

    store.push((count, expr));
}

#[cfg(test)]
mod tests {
    use crate::simplify::add::{coefficient_expression_split, search_and_update_count};
    use crate::Expression;

    #[test]
    fn test_add_simplification() {
        // integer collection
        // assert_eq!(
        //     Expression::Integer(1) + Expression::Integer(2),
        //     Expression::Integer(3)
        // );
        //
        // // integer collection, mixed with variables
        // assert_eq!(
        //     (Expression::Integer(1)
        //         + Expression::Variable("x".to_string())
        //         + Expression::Variable("y".to_string())
        //         + Expression::Integer(2))
        //     .to_string(),
        //     "(x + y + 3)"
        // );

        dbg!("new test");

        // integer + single variable collection
        assert_eq!(
            // 1 + 2 + 2x + y + x + x
            // 4x + y + 3
            (Expression::Integer(1)
                + Expression::Integer(2)
                + Expression::Mul(vec![
                    Expression::Variable("x".to_string()),
                    Expression::Integer(2)
                ])
                + Expression::Variable("y".to_string())
                + Expression::Variable("x".to_string())
                + Expression::Variable("x".to_string()))
            .simplify()
            .to_string(),
            "(4x + y + 3)"
        );

        // // 2 integers
        // assert_eq!(
        //     Expression::Add(vec![Expression::Integer(2), Expression::Integer(3)])
        //         .simplify()
        //         .to_string(),
        //     "5"
        // );
        //
        // // 4 integers
        // assert_eq!(
        //     Expression::Add(vec![
        //         Expression::Integer(2),
        //         Expression::Integer(3),
        //         Expression::Integer(4),
        //         Expression::Integer(5)
        //     ])
        //     .simplify()
        //     .to_string(),
        //     "14"
        // );
        //
        // // Integers mixed with variables
        // assert_eq!(
        //     Expression::Add(vec![
        //         Expression::Integer(3),
        //         Expression::Variable("x".to_string()),
        //         Expression::Integer(4),
        //         Expression::Variable("y".to_string())
        //     ])
        //     .simplify()
        //     .to_string(),
        //     "(x + y + 7)"
        // );
        //
        // assert_eq!(
        //     Expression::Add(vec![
        //         Expression::Add(vec![
        //             Expression::Variable("a".to_string()),
        //             Expression::Integer(2)
        //         ]),
        //         Expression::Add(vec![
        //             Expression::Integer(-2),
        //             Expression::Variable("b".to_string()),
        //             Expression::Integer(2)
        //         ]),
        //         Expression::Add(vec![
        //             Expression::Integer(2),
        //             Expression::Variable("c".to_string()),
        //             Expression::Integer(2)
        //         ]),
        //     ])
        //     .simplify()
        //     .to_string(),
        //     "(a + b + c + 6)"
        // );
    }

    #[test]
    fn test_coefficient_split() {
        assert_eq!(
            coefficient_expression_split(Expression::Variable("x".to_string())),
            (1, Expression::Variable("x".to_string()))
        );
        assert_eq!(
            coefficient_expression_split(Expression::Mul(vec![
                Expression::Variable("x".to_string()),
                Expression::Variable("y".to_string())
            ])),
            (
                1,
                Expression::Mul(vec![
                    Expression::Variable("y".to_string()),
                    Expression::Variable("x".to_string())
                ])
            )
        );
        assert_eq!(
            coefficient_expression_split(Expression::Mul(vec![
                Expression::Variable("x".to_string()),
                Expression::Integer(3),
                Expression::Variable("y".to_string())
            ])),
            (
                3,
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
        search_and_update_count(&mut result, 2, Expression::Integer(3));
        assert_eq!(result.len(), 1);
        search_and_update_count(
            &mut result,
            -1,
            Expression::Add(vec![
                Expression::Integer(2),
                Expression::Variable("x".to_string()),
            ]),
        );
        assert_eq!(result.len(), 2);
        search_and_update_count(
            &mut result,
            1,
            Expression::Add(vec![
                Expression::Variable("x".to_string()),
                Expression::Integer(2),
            ]),
        );
        assert_eq!(result.len(), 2);
        assert_eq!(result[1].0, 0);
    }
}
