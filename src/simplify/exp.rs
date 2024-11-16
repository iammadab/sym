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
