use crate::Expression;

pub(crate) fn simplify_neg(expression: Expression) -> Expression {
    let child = expression.children()[0];

    // Substitution Rules
    // 1. Neg(Neg(x)) => x
    // 2. Neg(integer) => -1 * integer
    match child {
        Expression::Neg(inner_expr) => (**inner_expr).clone(),
        Expression::Integer(val) => Expression::Integer(-1 * val),
        _ => expression,
    }
}

pub(crate) fn simplify_inv(expression: Expression) -> Expression {
    let child = expression.children()[0];

    // Substitution Rules
    // Inv(Inv(x)) => x
    match child {
        Expression::Inv(inner_expr) => (**inner_expr).clone(),
        _ => expression,
    }
}
pub(crate) fn simplify_add(expression: Expression) -> Expression {
    todo!()
}

pub(crate) fn simplify_mul(expression: Expression) -> Expression {
    todo!()
}

#[cfg(test)]
mod tests {
    use crate::Expression;

    #[test]
    fn test_negation_simplification() {
        // Neg(Neg(a)) = a
        assert_eq!(
            Expression::Neg(Box::new(Expression::Neg(Box::new(Expression::Variable(
                "a"
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
    }

    #[test]
    fn test_inverse_simplification() {
        // Inv(Inv(a)) = a
        assert_eq!(
            Expression::Inv(Box::new(Expression::Inv(Box::new(Expression::Variable(
                "a"
            )))))
            .simplify()
            .to_string(),
            "a"
        );
    }
}
