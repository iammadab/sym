mod neg;
mod inv;
mod exp;
mod add;
mod mul;

pub(crate) use neg::simplify_neg;
pub(crate) use inv::simplify_inv;
pub(crate) use exp::simplify_exp;
pub(crate) use mul::simplify_mul;
pub(crate) use add::simplify_add;

#[cfg(test)]
mod tests {
    use crate::Expression;

    #[test]
    fn test_negation_simplification() {
        // Neg(Neg(a)) = a
        assert_eq!(
            Expression::Neg(Box::new(Expression::Neg(Box::new(Expression::Variable(
                "a".to_string()
            )))))
                .simplify()
                .to_string(),
            "a"
        );

        // Neg(2) = -2
        assert_eq!(
            Expression::Neg(Box::new(Expression::Integer(2)))
                .simplify()
                .to_string(),
            "-2"
        );

        // Neg(-2) = 2
        assert_eq!(
            Expression::Neg(Box::new(Expression::Integer(-2)))
                .simplify()
                .to_string(),
            "2"
        );

        // Neg(Add(a, b, c)) = Add(Neg(a), Neg(b), Neg(c))
        let (a, b, c) = (
            Expression::Variable("a".to_string()),
            Expression::Variable("b".to_string()),
            Expression::Variable("c".to_string()),
        );
        let expr = -(&a + &b + &c);
        assert_eq!(expr.simplify(), -a - b - c);
    }

    #[test]
    fn test_inverse_simplification() {
        // Inv(Inv(a)) = a
        assert_eq!(
            Expression::Inv(Box::new(Expression::Inv(Box::new(Expression::Variable(
                "a".to_string()
            )))))
                .simplify()
                .to_string(),
            "a"
        );
    }

    #[test]
    fn test_exponentiation_simplification() {
        let x = Expression::Variable("x".to_string());

        let x_to_0 = x.pow(&Expression::Integer(0)).simplify();
        assert_eq!(x_to_0, Expression::Integer(1));

        let x_to_1 = x.pow(&Expression::Integer(1)).simplify();
        assert_eq!(x_to_1.to_string(), "x");

        let zero_to_x = Expression::Integer(0).pow(&x).simplify();
        assert_eq!(zero_to_x.to_string(), "0");
    }

    #[test]
    fn test_add_simplification() {
        // 2 integers
        assert_eq!(
            Expression::Add(vec![Expression::Integer(2), Expression::Integer(3)])
                .simplify()
                .to_string(),
            "5"
        );

        // 4 integers
        assert_eq!(
            Expression::Add(vec![
                Expression::Integer(2),
                Expression::Integer(3),
                Expression::Integer(4),
                Expression::Integer(5)
            ])
                .simplify()
                .to_string(),
            "14"
        );

        // Integers mixed with variables
        assert_eq!(
            Expression::Add(vec![
                Expression::Integer(3),
                Expression::Variable("x".to_string()),
                Expression::Integer(4),
                Expression::Variable("y".to_string())
            ])
                .simplify()
                .to_string(),
            "(x + y + 7)"
        );

        assert_eq!(
            Expression::Add(vec![
                Expression::Add(vec![
                    Expression::Variable("a".to_string()),
                    Expression::Integer(2)
                ]),
                Expression::Add(vec![
                    Expression::Integer(-2),
                    Expression::Variable("b".to_string()),
                    Expression::Integer(2)
                ]),
                Expression::Add(vec![
                    Expression::Integer(2),
                    Expression::Variable("c".to_string()),
                    Expression::Integer(2)
                ]),
            ])
                .simplify()
                .to_string(),
            "(a + b + c + 6)"
        );
    }

    #[test]
    fn test_mul_simplification() {
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
