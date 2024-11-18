use crate::Expression;
use std::ops::DivAssign;

pub(crate) fn simplify_mul(expression: Expression) -> Expression {
    // simplify each term in the mul expression
    let terms = expression.children().into_iter().map(|c| c.simplify());

    // collapse child multiplication terms i.e (a * b) * c = a * b * c
    let terms = terms
        .flat_map(|child| match child {
            Expression::Mul(_) => child.children(),
            _ => vec![child],
        })
        .collect::<Vec<_>>();

    // handle distribution
    // (a + b) * c = ac + bc
    let (addition_node, terms) = partition_at_addition_node(terms);
    if addition_node.is_some() {
        let terms_as_mul_node = Expression::Mul(terms);
        return Expression::Add(
            addition_node
                .unwrap()
                .children()
                .into_iter()
                .map(|c| c * &terms_as_mul_node)
                .collect(),
        )
        .simplify();
    }

    // remove neg but track parity
    let mut is_negative = false;
    let terms = terms
        .into_iter()
        .map(|t| match t {
            Expression::Neg(inner) => {
                is_negative = !is_negative;
                *inner
            }
            Expression::Inv(ref child) => match &**child {
                Expression::Neg(inner) => {
                    is_negative = !is_negative;
                    Expression::Inv(inner.clone())
                }
                _ => t,
            },
            _ => t,
        })
        .collect::<Vec<_>>();

    // collect integers, collect numerator and denominator
    let mut fraction_prod = Expression::multiplicative_identity();
    let terms = terms
        .into_iter()
        .filter(|t| match t {
            Expression::Fraction(..) => {
                fraction_prod = multiply_fraction(&fraction_prod, &t);
                false
            }
            _ => true,
        })
        .collect::<Vec<_>>();

    // early return if the fraction component will resolve to 0
    if fraction_prod == Expression::integer(0) {
        return fraction_prod;
    }

    // collect variable terms (exponentiation + automatic term cancellation)
    let mut variable_map = vec![];
    for term in terms {
        let term_power_expression = power_expression_split(term);
        search_and_update_expr_count(
            &mut variable_map,
            term_power_expression.0,
            term_power_expression.1,
        );
    }

    // convert the variable back into terms
    let mut variable_map_rewrite_terms = vec![];
    for (power_expression, expr) in variable_map {
        variable_map_rewrite_terms
            .push(Expression::Exp(Box::new(expr), Box::new(power_expression)).simplify());
    }

    // build final terms
    let mut final_terms = vec![];
    if fraction_prod != Expression::integer(1) {
        final_terms.push(fraction_prod);
    }
    final_terms.extend(variable_map_rewrite_terms);

    let final_expression = if final_terms.len() == 0 {
        // if no term remain (i.e. all terms cancelled out) then return 1
        Expression::integer(1)
    } else if final_terms.len() == 1 {
        // if only one term left no need to rap term in Mul
        final_terms.pop().unwrap()
    } else {
        Expression::Mul(final_terms)
    };

    if is_negative {
        Expression::Neg(Box::new(final_expression)).simplify()
    } else {
        final_expression
    }
}

fn partition_at_addition_node(mut terms: Vec<Expression>) -> (Option<Expression>, Vec<Expression>) {
    // remove the first add node we find
    // return that and return rest
    let mut addition_node = None;
    if let Some(pos) = terms.iter().position(|x| matches!(x, Expression::Add(_))) {
        addition_node = Some(terms.remove(pos));
    }
    (addition_node, terms)
}

// returns -> (power, expression)
fn power_expression_split(expr: Expression) -> (Expression, Expression) {
    match expr {
        Expression::Exp(base, power) => (*power, *base),
        Expression::Inv(expr) => {
            let (expr_power, expr_body) = power_expression_split(*expr);
            (Expression::integer(-1) * expr_power, expr_body)
        }
        _ => (Expression::integer(1), expr),
    }
}

fn search_and_update_expr_count(
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
    store.push((count, expr))
}

fn multiply_fraction(a: &Expression, b: &Expression) -> Expression {
    let (a_num, a_denom) = a.decompose_fraction().unwrap();
    let (b_num, b_denom) = b.decompose_fraction().unwrap();
    Expression::Fraction(a_num * b_num, a_denom * b_denom).simplify()
}

#[cfg(test)]
mod tests {
    use crate::simplify::mul::{
        partition_at_addition_node, power_expression_split, search_and_update_expr_count,
    };
    use crate::Expression;

    #[test]
    fn test_mul_simplification() {
        // flatten mul expressions
        assert_eq!(
            Expression::Mul(vec![
                Expression::Variable("x".to_string()),
                Expression::Mul(vec![
                    Expression::Variable("z".to_string()),
                    Expression::Variable("y".to_string())
                ])
            ])
            .simplify(),
            Expression::Mul(vec![
                Expression::Variable("x".to_string()),
                Expression::Variable("y".to_string()),
                Expression::Variable("z".to_string())
            ])
        );
    }

    #[test]
    fn test_integer_division() {
        assert_eq!(
            Expression::integer(1) / Expression::integer(1),
            Expression::integer(1)
        );
        assert_eq!(
            Expression::integer(1) / Expression::integer(2),
            Expression::Inv(Box::new(Expression::integer(2))).simplify()
        );
        assert_eq!(
            Expression::integer(-1) / Expression::integer(-1),
            Expression::integer(1)
        );
        assert_eq!(
            Expression::integer(-1) / Expression::integer(1),
            Expression::integer(-1)
        );
        assert_eq!(
            Expression::integer(4) / Expression::integer(2),
            Expression::integer(2)
        );
        assert_eq!(
            Expression::integer(8) / Expression::integer(6),
            Expression::integer(4) / Expression::integer(3)
        );
    }

    #[test]
    fn test_distribution() {
        // (x + y) * z = xz + yz
        assert_eq!(
            Expression::Mul(vec![
                Expression::Add(vec![
                    Expression::Variable("x".to_string()),
                    Expression::Variable("y".to_string())
                ]),
                Expression::Variable("z".to_string())
            ])
            .simplify()
            .to_string(),
            "(xz + yz)"
        );

        // (a + b) * (c + d)
        // ac + ad + bc + bd
        let (a, b, c, d) = (
            Expression::Variable("a".to_string()),
            Expression::Variable("b".to_string()),
            Expression::Variable("c".to_string()),
            Expression::Variable("d".to_string()),
        );
        assert_eq!((&a + &b) * (&c + &d), &a * &c + a * &d + &b * c + &b * &d);
    }

    #[test]
    fn test_partition_at_addition_node() {
        assert_eq!(
            partition_at_addition_node(vec![
                Expression::Add(vec![
                    Expression::Variable("x".to_string()),
                    Expression::Variable("y".to_string())
                ]),
                Expression::Variable("z".to_string()),
                Expression::Add(vec![
                    Expression::Variable("m".to_string()),
                    Expression::Variable("n".to_string())
                ]),
            ]),
            (
                Some(Expression::Add(vec![
                    Expression::Variable("x".to_string()),
                    Expression::Variable("y".to_string())
                ])),
                vec![
                    Expression::Variable("z".to_string()),
                    Expression::Add(vec![
                        Expression::Variable("m".to_string()),
                        Expression::Variable("n".to_string())
                    ])
                ]
            )
        )
    }

    #[test]
    fn test_power_expression_split() {
        // x^2
        assert_eq!(
            power_expression_split(
                Expression::Variable("x".to_string()).pow(&Expression::integer(2))
            ),
            (
                Expression::integer(2),
                Expression::Variable("x".to_string())
            )
        );

        // 1 / (x^2) = x^-2
        assert_eq!(
            power_expression_split(Expression::Inv(Box::new(
                Expression::Variable("x".to_string()).pow(&Expression::integer(2))
            ))),
            (
                Expression::integer(-2),
                Expression::Variable("x".to_string())
            )
        );

        // x
        assert_eq!(
            power_expression_split(Expression::Variable("x".to_string())),
            (
                Expression::integer(1),
                Expression::Variable("x".to_string())
            )
        );

        // x^-1
        assert_eq!(
            power_expression_split(Expression::Inv(Box::new(Expression::Variable(
                "x".to_string()
            )))),
            (
                Expression::integer(-1),
                Expression::Variable("x".to_string())
            )
        );
    }

    #[test]
    fn test_search_and_update_expr() {
        let mut result = vec![];
        search_and_update_expr_count(&mut result, Expression::integer(2), Expression::integer(3));
        assert_eq!(result.len(), 1);
        search_and_update_expr_count(
            &mut result,
            Expression::integer(-3),
            Expression::Variable("x".to_string()),
        );
        assert_eq!(result.len(), 2);
        search_and_update_expr_count(
            &mut result,
            Expression::integer(2),
            Expression::Variable("x".to_string()),
        );
        assert_eq!(result.len(), 2);
        assert_eq!(result[1].0, Expression::integer(-1));
    }
}
