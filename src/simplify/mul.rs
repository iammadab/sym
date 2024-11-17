use crate::Expression;

pub(crate) fn simplify_mul(expression: Expression) -> Expression {
    let terms = expression.children().into_iter().map(|c| c.simplify());

    let terms = terms.flat_map(|child| match child {
        Expression::Mul(_) => child.children(),
        _ => vec![child],
    });

    // remove neg but track parity
    let mut is_negative = false;
    let terms = terms
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

#[cfg(test)]
mod tests {
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
    fn test_old_mul_simplification() {
        // Integers mixed with variables
        assert_eq!(
            Expression::Mul(vec![
                Expression::Integer(3),
                Expression::Variable("x".to_string()),
                Expression::Integer(4),
                Expression::Variable("y".to_string())
            ])
            .simplify()
            .to_string(),
            "12xy"
        );

        assert_eq!(
            Expression::Mul(vec![
                Expression::Mul(vec![
                    Expression::Variable("a".to_string()),
                    Expression::Integer(2)
                ]),
                Expression::Mul(vec![
                    Expression::Integer(-2),
                    Expression::Variable("b".to_string()),
                    Expression::Integer(2)
                ]),
                Expression::Mul(vec![
                    Expression::Integer(2),
                    Expression::Variable("c".to_string()),
                    Expression::Integer(2)
                ]),
            ])
            .simplify()
            .to_string(),
            "-32abc"
        );
    }
}
