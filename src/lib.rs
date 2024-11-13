mod airth_macros;
mod simplify;

use crate::simplify::{simplify_add, simplify_inv, simplify_mul, simplify_neg};
use std::fmt::{Display, Formatter, Write};

#[derive(Clone, Debug, PartialEq)]
enum Expression {
    Variable(String),
    Integer(isize),
    Neg(Box<Expression>),
    Inv(Box<Expression>),
    Add(Vec<Expression>),
    Mul(Vec<Expression>),
}

impl Expression {
    fn substitute(&self, substitution_map: &[(String, isize)]) -> Self {
        match self {
            Expression::Integer(_) => self.clone(),
            Expression::Variable(var_name) => {
                for (var, val) in substitution_map {
                    if var == var_name {
                        return Self::Integer(*val);
                    }
                }
                self.clone()
            }
            Expression::Neg(expr) => Expression::Neg(Box::new(expr.substitute(substitution_map))),
            Expression::Inv(expr) => Expression::Inv(Box::new(expr.substitute(substitution_map))),
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
            Expression::Integer(val) => *val as f64,
            Expression::Neg(expr) => expr.evaluate(),
            Expression::Inv(expr) => 1.0 / expr.evaluate(),
            Expression::Add(exprs) => exprs.iter().fold(0.0, |acc, expr| acc + expr.evaluate()),
            Expression::Mul(exprs) => exprs.iter().fold(1.0, |acc, expr| acc * expr.evaluate()),
        }
    }

    fn simplify(self) -> Self {
        match self {
            Expression::Neg(_) => simplify_neg(self),
            Expression::Inv(_) => simplify_inv(self),
            Expression::Add(_) => simplify_add(self),
            Expression::Mul(_) => simplify_mul(self),
            _ => self,
        }
    }

    fn children(self) -> Vec<Self> {
        match self {
            Expression::Neg(expr) | Expression::Inv(expr) => vec![*expr],
            Expression::Add(exprs) | Expression::Mul(exprs) => exprs,
            _ => Vec::with_capacity(0),
        }
    }

    fn children_ref(&self) -> Vec<&Self> {
        match self {
            Expression::Neg(expr) | Expression::Inv(expr) => vec![expr],
            Expression::Add(exprs) | Expression::Mul(exprs) => exprs.iter().collect(),
            _ => Vec::with_capacity(0),
        }
    }

    fn as_integer(&self) -> Option<isize> {
        match self {
            Expression::Integer(value) => Some(*value),
            _ => None,
        }
    }

    fn multiplicative_identity() -> Expression {
        Expression::Integer(1)
    }

    fn additive_identity() -> Expression {
        Expression::Integer(0)
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Variable(var_name) => f.write_str(var_name),
            Expression::Integer(val) => f.write_str(val.to_string().as_str()),
            Expression::Neg(expr) => {
                f.write_str("-")?;
                expr.fmt(f)
            }
            Expression::Inv(expr) => {
                f.write_str("1/(")?;
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
                        Expression::Integer(val) => {
                            if val.is_negative() {
                                f.write_str(format!(" - {}", val.abs().to_string()).as_str())?;
                                continue;
                            }
                            f.write_str(format!(" + {}", expr.to_string()).as_str())?;
                        }
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
        let (two, three) = (Expression::Integer(2), Expression::Integer(3));

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
                    * ((&x - Expression::Integer(*v as isize))
                        / (Expression::Integer(*val as isize) - Expression::Integer(*v as isize)))
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
                .as_integer()
                .unwrap(),
            9
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

        let z = x - (y * Expression::Integer(2));
        assert_eq!(z.to_string(), "(x - 2y)");
    }

    #[test]
    fn test_basic_simplification_on_init() {
        let a = Expression::Integer(2) + Expression::Integer(3);
        assert_eq!(a.to_string(), "5");
    }

    #[test]
    fn test_division() {
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
    fn test_lagrange_simplification() {
        let linear_interpolation = lagrange_expression(&[0, 1]);
        assert_eq!(linear_interpolation.to_string(), "");
    }
}
