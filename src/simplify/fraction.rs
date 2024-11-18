use crate::Expression;

pub(crate) fn simplify_fraction(expression: Expression) -> Expression {
    let (numerator, denominator) = expression.decompose_fraction().unwrap();
    todo!()
}

#[cfg(test)]
mod tests {
    use crate::Expression;

    #[test]
    fn test_fraction_simplification() {
        assert_eq!(
            Expression::Fraction(1, 1).simplify(),
            Expression::Fraction(1, 1)
        );
        assert_eq!(
            Expression::Fraction(1, -1).simplify(),
            Expression::Fraction(-1, 1)
        );
        assert_eq!(
            Expression::Fraction(-1, -1).simplify(),
            Expression::Fraction(1, 1)
        );
        assert_eq!(
            Expression::Fraction(2, 2).simplify(),
            Expression::Fraction(1, 1)
        );
        assert_eq!(
            Expression::Fraction(4, 2).simplify(),
            Expression::Fraction(2, 1)
        );
        assert_eq!(
            Expression::Fraction(2, 4).simplify(),
            Expression::Fraction(1, 2)
        );
        assert_eq!(
            Expression::Fraction(2, 3).simplify(),
            Expression::Fraction(2, 3)
        );
    }
}
