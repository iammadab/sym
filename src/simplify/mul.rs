use crate::Expression;

pub(crate) fn simplify_mul(expression: Expression) -> Expression {
    let terms = expression.children().into_iter().map(|c| c.simplify());

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
    let mut numerator_prod = 1;
    let mut denominator_prod = 1;
    let terms = terms
        .into_iter()
        .filter(|t| match t {
            Expression::Integer(val) => {
                numerator_prod *= val;
                false
            }
            Expression::Inv(inner) => match &**inner {
                Expression::Integer(val) => {
                    denominator_prod *= val;
                    false
                }
                _ => true,
            },
            _ => true,
        })
        .collect::<Vec<_>>();

    Expression::Mul(terms)
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
            (Expression::Integer(-1) * expr_power, expr_body)
        }
        _ => (Expression::Integer(1), expr),
    }
}

#[cfg(test)]
mod tests {
    use crate::simplify::mul::{partition_at_addition_node, power_expression_split};
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
                Expression::Variable("x".to_string()).pow(&Expression::Integer(2))
            ),
            (
                Expression::Integer(2),
                Expression::Variable("x".to_string())
            )
        );

        // 1 / (x^2) = x^-2
        assert_eq!(
            power_expression_split(Expression::Inv(Box::new(
                Expression::Variable("x".to_string()).pow(&Expression::Integer(2))
            ))),
            (
                Expression::Integer(2),
                Expression::Variable("x".to_string())
            )
        );

        // x
        assert_eq!(
            power_expression_split(Expression::Variable("x".to_string())),
            (Expression::Integer(1), Expression::Variable("x".to_string()))
        );

        // x^-1
        assert_eq!(
            power_expression_split(Expression::Inv(Box::new(Expression::Variable("x".to_string())))),
            (Expression::Integer(-1), Expression::Variable("x".to_string()))
        );
    }
}
