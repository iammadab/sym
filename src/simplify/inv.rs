use crate::Expression;

pub(crate) fn simplify_inv(expression: Expression) -> Expression {
    let child = expression.children().pop().unwrap().simplify();

    // Substitution Rules
    // Inv(Inv(x)) => x
    match child {
        Expression::Inv(inner_expr) => inner_expr.simplify(),
        _ => Expression::Inv(Box::new(child)),
    }
}

#[cfg(test)]
mod tests {
    use crate::Expression;

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
}