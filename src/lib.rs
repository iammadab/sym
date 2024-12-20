mod airth_macros;
mod simplify;

use crate::simplify::{
    simplify_add, simplify_exp, simplify_fraction, simplify_inv, simplify_mul, simplify_neg,
};
use std::fmt::{Display, Formatter, Write};
use std::ops::Neg;

#[derive(Clone, Debug)]
enum Expression {
    Variable(String),
    // fraction(numerator, denominator)
    Fraction(isize, isize),
    Neg(Box<Expression>),
    Inv(Box<Expression>),
    // expr(base, exponent)
    Exp(Box<Expression>, Box<Expression>),
    Add(Vec<Expression>),
    Mul(Vec<Expression>),
}

impl Expression {
    fn substitute(&self, substitution_map: &[(String, isize)]) -> Self {
        match self {
            Expression::Fraction(..) => self.clone(),
            Expression::Variable(var_name) => {
                for (var, val) in substitution_map {
                    if var == var_name {
                        return Self::integer(*val);
                    }
                }
                self.clone()
            }
            Expression::Neg(expr) => Expression::Neg(Box::new(expr.substitute(substitution_map))),
            Expression::Inv(expr) => Expression::Inv(Box::new(expr.substitute(substitution_map))),
            Expression::Exp(base_expr, exponent_expr) => Expression::Exp(
                Box::new(base_expr.substitute(substitution_map)),
                Box::new(exponent_expr.substitute(substitution_map)),
            ),
            Expression::Add(exprs) => Expression::Add(
                exprs
                    .iter()
                    .map(|expr| expr.substitute(substitution_map))
                    .collect(),
            ),
            Expression::Mul(exprs) => Expression::Mul(
                exprs
                    .iter()
                    .map(|expr| expr.substitute(substitution_map))
                    .collect(),
            ),
        }
        .simplify()
    }

    fn evaluate(&self) -> f64 {
        match self {
            Expression::Variable(_) => {
                // variables shouldn't exist on evaluation call, panic
                panic!("cannot evaluate when free variable exists");
            }
            Expression::Fraction(numerator, denominator) => *numerator as f64 / *denominator as f64,
            Expression::Neg(expr) => expr.evaluate(),
            Expression::Inv(expr) => 1.0 / expr.evaluate(),
            Expression::Exp(base_expr, exponent_expr) => {
                base_expr.evaluate().powf(exponent_expr.evaluate())
            }
            Expression::Add(exprs) => exprs.iter().fold(0.0, |acc, expr| acc + expr.evaluate()),
            Expression::Mul(exprs) => exprs.iter().fold(1.0, |acc, expr| acc * expr.evaluate()),
        }
    }

    fn simplify(self) -> Self {
        match self {
            Expression::Fraction(..) => simplify_fraction(self),
            Expression::Neg(_) => simplify_neg(self),
            Expression::Inv(_) => simplify_inv(self),
            Expression::Exp(..) => simplify_exp(self),
            Expression::Add(_) => simplify_add(self),
            Expression::Mul(_) => simplify_mul(self),
            _ => self,
        }
    }

    fn pow(&self, exponent: &Expression) -> Self {
        Self::Exp(Box::new(self.clone()), Box::new(exponent.clone()))
    }

    fn children(self) -> Vec<Self> {
        match self {
            Expression::Neg(expr) | Expression::Inv(expr) => vec![*expr],
            Expression::Exp(base, exp) => vec![*base, *exp],
            Expression::Add(exprs) | Expression::Mul(exprs) => exprs,
            _ => Vec::with_capacity(0),
        }
    }

    fn integer(val: isize) -> Expression {
        Expression::Fraction(val, 1)
    }

    fn decompose_fraction(&self) -> Option<(isize, isize)> {
        match self {
            Expression::Fraction(numerator, denominator) => Some((*numerator, *denominator)),
            _ => None,
        }
    }

    fn multiplicative_identity() -> Expression {
        Expression::integer(1)
    }

    fn additive_identity() -> Expression {
        Expression::integer(0)
    }
}

impl PartialEq for Expression {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Expression::Variable(var1), Expression::Variable(var2)) => var1.eq(var2),
            (Expression::Fraction(num, denom), Expression::Fraction(num2, denom2)) => {
                num == num2 && denom == denom2
            }
            (Expression::Neg(expr1), Expression::Neg(expr2)) => expr1.eq(expr2),
            (Expression::Inv(expr1), Expression::Inv(expr2)) => expr1.eq(expr2),
            (Expression::Exp(base1, exponent1), Expression::Exp(base2, exponent2)) => {
                return base1.eq(base2) && exponent1.eq(exponent2);
            }
            (Expression::Mul(exprs1), Expression::Mul(exprs2))
            | (Expression::Add(exprs1), Expression::Add(exprs2)) => {
                if exprs1.len() != exprs2.len() {
                    return false;
                }

                let mut exprs2 = exprs2.clone();

                for expr in exprs1 {
                    if let Some(pos) = exprs2.iter().position(|expr2| expr.eq(expr2)) {
                        exprs2.remove(pos);
                    } else {
                        return false;
                    }
                }

                true
            }
            _ => false,
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Variable(var_name) => f.write_str(var_name),
            Expression::Fraction(numerator, denominator) => {
                f.write_str(format!("({numerator}/{denominator})").as_str())
            }
            Expression::Neg(expr) => {
                f.write_str("-")?;
                expr.fmt(f)
            }
            Expression::Inv(expr) => {
                f.write_str("1/(")?;
                expr.fmt(f)?;
                f.write_str(")")
            }
            Expression::Exp(base, expr) => {
                f.write_str("(")?;
                base.fmt(f)?;
                f.write_str("^")?;
                expr.fmt(f)?;
                f.write_str(")")
            }
            Expression::Add(exprs) => {
                f.write_str("(")?;

                let mut expr_iter = exprs.iter();

                // guaranteed that we have at least two expressions in the vec
                expr_iter.next().unwrap().fmt(f)?;

                for expr in expr_iter {
                    match expr {
                        Expression::Neg(expr) => {
                            f.write_str(format!(" - {}", expr.to_string()).as_str())?;
                            continue;
                        }
                        _ => {
                            f.write_str(format!(" + {}", expr.to_string()).as_str())?;
                        }
                    }
                }

                return f.write_str(")");
            }
            Expression::Mul(exprs) => {
                let mut expr_iter = exprs.iter();

                // guaranteed that we have at least two expressions in the vec
                expr_iter.next().unwrap().fmt(f)?;

                for expr in expr_iter {
                    f.write_str(format!("{}", expr.to_string()).as_str())?;
                }

                Ok(())
            }
        }
    }
}

impl Neg for Expression {
    type Output = Expression;

    fn neg(self) -> Self::Output {
        Expression::Neg(Box::new(self))
    }
}

#[cfg(test)]
mod tests {
    use crate::Expression;

    fn expr1() -> Expression {
        // 2x + 3y - z
        let (x, y, z) = (
            Expression::Variable("x".to_string()),
            Expression::Variable("y".to_string()),
            Expression::Variable("z".to_string()),
        );
        let (two, three) = (Expression::integer(2), Expression::integer(3));

        (two * x) + (three * y) - z
    }

    fn lagrange_expression(interpolating_set: &[usize]) -> Expression {
        let x = Expression::Variable("x".to_string());
        let mut term = Expression::additive_identity();

        for val in interpolating_set {
            let y = Expression::Variable(format!("y{}", val));
            let mut basis = Expression::multiplicative_identity();
            for v in interpolating_set {
                if v == val {
                    continue;
                }

                basis = basis
                    * ((&x - Expression::integer(*v as isize))
                        / (Expression::integer(*val as isize) - Expression::integer(*v as isize)))
            }
            term = term + (y * basis)
        }

        term
    }

    #[test]
    fn test_expression_creation() {
        assert_eq!(expr1().to_string(), "(2x + 3y - z)");
    }

    #[test]
    fn test_expression_evaluation() {
        // x = 2, y = 3, z = 4
        // 2.2 + 3.3 - 4
        // 4 + 9 - 4 = 9
        assert_eq!(
            expr1()
                .substitute(&[
                    ("x".to_string(), 2),
                    ("y".to_string(), 3),
                    ("z".to_string(), 4)
                ])
                .evaluate(),
            9.0
        );

        // x / y where x = 1 and y = 2
        let (x, y) = (
            Expression::Variable("x".to_string()),
            Expression::Variable("y".to_string()),
        );
        let x_div_y = x / y;
        let simplified = x_div_y.substitute(&[("x".to_string(), 1), ("y".to_string(), 2)]);
        let result = simplified.evaluate();
        assert_eq!(result, 0.5);
    }

    #[test]
    fn test_expression_substitution() {
        // z = 2
        let expr = expr1().substitute(&[("z".to_string(), 2)]);
        assert_eq!(expr.to_string(), "(2x + 3y - 2)");

        // m = 2 <- no-op
        let expr = expr.substitute(&[("m".to_string(), 3)]);
        assert_eq!(expr.to_string(), "(2x + 3y - 2)");

        // y = 3
        let expr = expr.substitute(&[("y".to_string(), 3)]);
        assert_eq!(expr.to_string(), "(2x + 7)");

        // x = 4
        let expr = expr.substitute(&[("x".to_string(), 4)]);
        assert_eq!(expr.to_string(), "15");
    }

    #[test]
    fn test_different_atom_combinations() {
        let (x, y) = (
            Expression::Variable("x".to_string()),
            Expression::Variable("y".to_string()),
        );
        let a = &x + &y;
        let b = x.clone() + y.clone();
        let c = &x + y.clone();
        let d = x + &y;
        assert_eq!(a, b);
        assert_eq!(b, c);
        assert_eq!(c, d);
    }

    #[test]
    fn test_negation_display() {
        let (x, y) = (
            Expression::Variable("x".to_string()),
            Expression::Variable("y".to_string()),
        );
        let z = &x - &y;
        assert_eq!(z.to_string(), "(x - y)");

        let z = x - (y * Expression::integer(2));
        assert_eq!(z.to_string(), "(x - 2y)");
    }

    #[test]
    fn test_basic_simplification_on_init() {
        let a = Expression::integer(2) + Expression::integer(3);
        assert_eq!(a, Expression::integer(5));
    }

    #[test]
    fn test_division_display() {
        let (x, y) = (
            Expression::Variable("x".to_string()),
            Expression::Variable("y".to_string()),
        );
        let div_expr = x / y;
        assert_eq!(div_expr.to_string(), "x/(y)");

        let div_expr = div_expr.substitute(&[("x".to_string(), 4)]);
        assert_eq!(div_expr.to_string(), "4/(y)");

        let div_expr = div_expr.substitute(&[("y".to_string(), 2)]);
        assert_eq!(div_expr.to_string(), "4/(2)");
    }

    #[test]
    fn test_exponetiation() {
        assert_eq!(
            Expression::Exp(
                Box::new(Expression::integer(2)),
                Box::new(Expression::integer(3))
            )
            .evaluate(),
            8.0
        );

        let (x, y) = (
            Expression::Variable("x".to_string()),
            Expression::Variable("y".to_string()),
        );
        let exp_expr = x.pow(&y);
        assert_eq!(exp_expr.to_string(), "(x^y)");
    }

    #[test]
    fn test_lagrange_simplification() {
        let linear_interpolation = lagrange_expression(&[0, 1]);
        assert_eq!(linear_interpolation.to_string(), "");
    }

    #[test]
    fn test_expression_compare() {
        assert_eq!(Expression::integer(2), Expression::integer(2));
        assert_ne!(Expression::integer(2), Expression::integer(3));

        assert_eq!(
            Expression::Variable("x".to_string()),
            Expression::Variable("x".to_string())
        );
        assert_ne!(
            Expression::Variable("x".to_string()),
            Expression::Variable("y".to_string())
        );

        assert_eq!(
            Expression::Inv(Box::new(Expression::integer(2))),
            Expression::Inv(Box::new(Expression::integer(2)))
        );

        assert_eq!(
            Expression::Neg(Box::new(Expression::Variable("x".to_string()))),
            Expression::Neg(Box::new(Expression::Variable("x".to_string())))
        );

        assert_eq!(
            Expression::Exp(
                Box::new(Expression::Variable("x".to_string())),
                Box::new(Expression::integer(2))
            ),
            Expression::Exp(
                Box::new(Expression::Variable("x".to_string())),
                Box::new(Expression::integer(2))
            )
        );

        assert_eq!(
            Expression::Add(vec![
                Expression::integer(2),
                Expression::Variable("x".to_string()),
                Expression::Exp(
                    Box::new(Expression::Variable("x".to_string())),
                    Box::new(Expression::Variable("y".to_string()))
                )
            ]),
            Expression::Add(vec![
                Expression::Exp(
                    Box::new(Expression::Variable("x".to_string())),
                    Box::new(Expression::Variable("y".to_string()))
                ),
                Expression::integer(2),
                Expression::Variable("x".to_string())
            ])
        );

        assert_eq!(
            Expression::Mul(vec![
                Expression::integer(2),
                Expression::Variable("x".to_string()),
                Expression::Exp(
                    Box::new(Expression::Variable("x".to_string())),
                    Box::new(Expression::Variable("y".to_string()))
                )
            ]),
            Expression::Mul(vec![
                Expression::Exp(
                    Box::new(Expression::Variable("x".to_string())),
                    Box::new(Expression::Variable("y".to_string()))
                ),
                Expression::integer(2),
                Expression::Variable("x".to_string())
            ])
        );
    }
}
