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
