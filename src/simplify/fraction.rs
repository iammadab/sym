use crate::Expression;

pub(crate) fn simplify_fraction(expression: Expression) -> Expression {
    let (numerator, denominator) = expression.decompose_fraction().unwrap();
    todo!()
}
