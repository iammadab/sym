use crate::Expression;

pub(crate) fn simplify_exp(expression: Expression) -> Expression {
    // Substitution Rules
    // 1. expr^0 = 1
    // 2. 0^expr = 0
    // 3. expr^1 = expr

    let mut children = expression.children();
    let exponent = children.pop().unwrap().simplify();
    let base = children.pop().unwrap().simplify();

    if matches!(base, Expression::Integer(0)) {
        return Expression::Integer(0);
    }

    if matches!(exponent, Expression::Integer(0)) {
        return Expression::Integer(1);
    }

    if matches!(exponent, Expression::Integer(1)) {
        return base;
    }

    return Expression::Exp(Box::new(base), Box::new(exponent));
}

#[cfg(test)]
mod tests {
    use crate::Expression;

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
}
