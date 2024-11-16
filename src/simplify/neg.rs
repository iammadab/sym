use crate::Expression;

pub(crate) fn simplify_neg(expression: Expression) -> Expression {
    let neg_inner = expression.children().pop().unwrap().simplify();

    // Substitution Rules
    // 1. Neg(Neg(x)) => x
    // 2. Neg(integer) => -1 * integer
    // 3. Neg(a + b + c) => Neg(a) + Neg(b) + Neg(c)
    match neg_inner {
        Expression::Neg(inner_expr) => (*inner_expr).clone(),
        Expression::Integer(val) => Expression::Integer(-1 * val),
        Expression::Add(_) => {
            let add_terms = neg_inner.children();
            Expression::Add(
                add_terms
                    .into_iter()
                    .map(|t| Expression::Neg(Box::new(t)))
                    .collect(),
            )
                .simplify()
        }
        _ => Expression::Neg(Box::new(neg_inner)),
    }
}
