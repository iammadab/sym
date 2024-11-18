use crate::Expression;

pub(crate) fn simplify_neg(expression: Expression) -> Expression {
    let neg_inner = expression.children().pop().unwrap().simplify();

    // Substitution Rules
    // 1. Neg(Neg(x)) => x
    // 2. Neg(integer) => -1 * integer
    // 3. Neg(a + b + c) => Neg(a) + Neg(b) + Neg(c)
    // 4. Neg(Frac(a, b)) => Frac(-a, b)
    match neg_inner {
        Expression::Neg(inner_expr) => (*inner_expr).clone(),
        Expression::Fraction(a, b) => Expression::Fraction(-1 * a, b),
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
            Expression::Neg(Box::new(Expression::integer(2))).simplify(),
            Expression::integer(-2)
        );

        // Neg(-2) = 2
        assert_eq!(
            Expression::Neg(Box::new(Expression::integer(-2))).simplify(),
            Expression::integer(2)
        );

        // Neg(1/2) = -1 / 2
        assert_eq!(
            Expression::Neg(Box::new(Expression::Fraction(1, 2))).simplify(),
            Expression::Fraction(-1, 2)
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
}
