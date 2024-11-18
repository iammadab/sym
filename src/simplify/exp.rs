use crate::Expression;

pub(crate) fn simplify_exp(expression: Expression) -> Expression {
    // Substitution Rules
    // 1. expr^0 = 1
    // 2. 0^expr = 0
    // 3. expr^1 = expr

    let mut children = expression.children();
    let exponent = children.pop().unwrap().simplify();
    let base = children.pop().unwrap().simplify();

    if matches!(base, Expression::Fraction(0, 1)) {
        return Expression::integer(0);
    }

    if matches!(exponent, Expression::Fraction(0, 1)) {
        return Expression::integer(1);
    }

    if matches!(exponent, Expression::Fraction(1, 1)) {
        return base;
    }

    if matches!(base, Expression::Fraction(..)) && matches!(exponent, Expression::Fraction(..)) {
        let (mut base_num, mut base_denom) = base.decompose_fraction().unwrap();
        let (mut exp_num, mut exp_denom) = exponent.decompose_fraction().unwrap();

        // determine if fraction parity
        // a b -> c (XOR)
        // f f -> t
        // f t -> f
        // t f -> f
        // t t -> t
        let is_negative = exp_num.is_positive() ^ exp_denom.is_positive();

        // remove sign for exponent
        exp_num = exp_num.abs();
        exp_denom = exp_denom.abs();

        // if exponent denominator == 1, compute exponentiation and perform node rewrite
        if exp_denom == 1 {
            base_num = base_num.pow(exp_num as u32);
            base_denom = base_denom.pow(exp_num as u32);
            return if is_negative {
                Expression::Fraction(base_denom, base_num)
            } else {
                Expression::Fraction(base_num, base_denom)
            };
        }

        return if is_negative {
            Expression::Exp(
                Box::new(Expression::Fraction(base_denom, base_num)),
                Box::new(Expression::Fraction(exp_num, exp_denom)),
            )
        } else {
            Expression::Exp(
                Box::new(Expression::Fraction(base_num, base_denom)),
                Box::new(Expression::Fraction(exp_num, exp_denom)),
            )
        };
    }

    return Expression::Exp(Box::new(base), Box::new(exponent));
}

#[cfg(test)]
mod tests {
    use crate::Expression;

    #[test]
    fn test_exponentiation_simplification() {
        let x = Expression::Variable("x".to_string());

        let x_to_0 = x.pow(&Expression::integer(0)).simplify();
        assert_eq!(x_to_0, Expression::integer(1));

        let x_to_1 = x.pow(&Expression::integer(1)).simplify();
        assert_eq!(x_to_1.to_string(), "x");

        let zero_to_x = Expression::integer(0).pow(&x).simplify();
        assert_eq!(zero_to_x, Expression::integer(0));
    }

    #[test]
    fn test_exp_frac_simplification() {
        // cases
        assert_eq!(
            Expression::Fraction(2, 3)
                .pow(&Expression::Fraction(1, 1))
                .simplify(),
            Expression::Fraction(2, 3)
        );
        assert_eq!(
            Expression::Fraction(2, 3)
                .pow(&Expression::Fraction(-1, 1))
                .simplify(),
            Expression::Fraction(3, 2)
        );
        assert_eq!(
            Expression::Fraction(2, 3)
                .pow(&Expression::Fraction(1, -1))
                .simplify(),
            Expression::Fraction(3, 2)
        );
        assert_eq!(
            Expression::Fraction(2, 3)
                .pow(&Expression::Fraction(-1, -1))
                .simplify(),
            Expression::Fraction(2, 3)
        );

        assert_eq!(
            Expression::Fraction(2, 3)
                .pow(&Expression::Fraction(1, 2))
                .simplify(),
            Expression::Exp(
                Box::new(Expression::Fraction(2, 3)),
                Box::new(Expression::Fraction(1, 2))
            )
        );

        assert_eq!(
            Expression::Fraction(2, 3)
                .pow(&Expression::Fraction(2, 1))
                .simplify(),
            Expression::Fraction(4, 9)
        );

        assert_eq!(
            Expression::Fraction(2, 3)
                .pow(&Expression::Fraction(8, 4))
                .simplify(),
            Expression::Fraction(4, 9)
        );
        assert_eq!(
            Expression::Fraction(2, 3)
                .pow(&Expression::Fraction(8, -4))
                .simplify(),
            Expression::Fraction(9, 4)
        );
    }
}
