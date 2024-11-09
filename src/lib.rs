mod airth_macros;

use std::fmt::{Display, Formatter, Write};

#[derive(Clone, Debug, PartialEq)]
enum Expression {
    Variable(&'static str),
    Integer(isize),
    Neg(Box<Expression>),
    Inv(Box<Expression>),
    Add(Box<Expression>, Box<Expression>),
    Mul(Box<Expression>, Box<Expression>),
}

impl Expression {
    fn neg(expression: Self) -> Self {
        if let Some(val) = expression.as_integer() {
            return Expression::Integer(-1 * val).into();
        }

        Expression::Neg(Box::new(expression))
    }

    fn inv(expression: Self) -> Self {
        Expression::Inv(Box::new(expression))
    }

    fn add(left: Self, right: Self) -> Self {
        if let Some(left_val) = left.as_integer() {
            if let Some(right_val) = right.as_integer() {
                return Expression::Integer(left_val + right_val).into();
            }
        }

        Expression::Add(Box::new(left), Box::new(right))
    }

    fn mul(left: Self, right: Self) -> Self {
        if let Some(left_val) = left.as_integer() {
            if let Some(right_val) = right.as_integer() {
                return Expression::Integer(left_val * right_val).into();
            }
        }

        Expression::Mul(Box::new(left), Box::new(right))
    }

    fn substitute(&self, substitution_map: &[(&'static str, isize)]) -> Self {
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
            Expression::Neg(expr) => Expression::neg(expr.substitute(substitution_map)),
            Expression::Inv(expr) => Expression::inv(expr.substitute(substitution_map)),
            Expression::Add(left, right) => Expression::add(
                left.substitute(substitution_map),
                right.substitute(substitution_map),
            ),
            Expression::Mul(left, right) => Expression::mul(
                left.substitute(substitution_map),
                right.substitute(substitution_map),
            ),
        }
    }

    fn as_integer(&self) -> Option<isize> {
        match self {
            Expression::Integer(value) => Some(*value),
            _ => None,
        }
    }

    fn long_form_negation(&self) -> Option<String> {
        match self {
            Expression::Neg(expr) => match &**expr {
                Expression::Variable(var_name) => Some(format!(" - {}", var_name).to_string()),
                _ => None,
            },
            _ => None,
        }
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
                f.write_str("(")?;
                expr.fmt(f)?;
                f.write_str(")^-1")
            }
            Expression::Add(left, right) => {
                f.write_str("(")?;
                left.fmt(f)?;

                match &**right {
                    Expression::Integer(val) => {
                        if val.is_negative() {
                            return f.write_str(format!(" - {})", val.abs().to_string()).as_str());
                        }
                    }
                    Expression::Neg(expr) => {
                        if let Some(negation_str) = right.long_form_negation() {
                            f.write_str(negation_str.as_str())?;
                            return f.write_str(")");
                        }

                        // for negated expressions that are not variables or integers
                        f.write_str(" - ")?;
                        expr.fmt(f)?;
                        return f.write_str(")");
                    }
                    _ => {}
                }

                f.write_str(" + ")?;
                right.fmt(f)?;
                f.write_str(")")
            }
            Expression::Mul(left, right) => {
                f.write_str("(")?;
                left.fmt(f)?;
                f.write_str(" * ")?;
                right.fmt(f)?;
                f.write_str(")")
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
            Expression::Variable("x"),
            Expression::Variable("y"),
            Expression::Variable("z"),
        );
        let (two, three) = (Expression::Integer(2), Expression::Integer(3));

        (two * x) + (three * y) - z
    }

    #[test]
    fn test_expression_creation() {
        assert_eq!(expr1().to_string(), "(((2 * x) + (3 * y)) - z)");
    }

    #[test]
    fn test_expression_evaluation() {
        // x = 2, y = 3, z = 4
        // 2.2 + 3.3 - 4
        // 4 + 9 - 4 = 9
        assert_eq!(
            expr1()
                .substitute(&[("x", 2), ("y", 3), ("z", 4)])
                .as_integer()
                .unwrap(),
            9
        );
    }

    #[test]
    fn test_expression_substitution() {
        // z = 2
        let expr = expr1().substitute(&[("z", 2)]);
        assert_eq!(expr.to_string(), "(((2 * x) + (3 * y)) - 2)");

        // m = 2 <- no-op
        let expr = expr.substitute(&[("m", 3)]);
        assert_eq!(expr.to_string(), "(((2 * x) + (3 * y)) - 2)");

        // y = 3
        let expr = expr.substitute(&[("y", 3)]);
        assert_eq!(expr.to_string(), "(((2 * x) + 9) - 2)");

        // x = 4
        let expr = expr.substitute(&[("x", 4)]);
        assert_eq!(expr.to_string(), "15");
    }

    #[test]
    fn test_different_atom_combinations() {
        let (x, y) = (Expression::Variable("x"), Expression::Variable("y"));
        let a = &x + &y;
        let b = x.clone() + y.clone();
        let c = &x + y.clone();
        let d = x + &y;
        assert_eq!(a, b);
        assert_eq!(b, c);
        assert_eq!(c, d);
    }

    #[test]
    fn test_long_form_negation() {
        assert_eq!(Expression::Integer(1).long_form_negation(), None);
        assert_eq!(
            Expression::neg(Expression::Integer(1)).long_form_negation(),
            None
        );
        assert_eq!(
            Expression::neg(Expression::Variable("x")).long_form_negation(),
            Some(" - x".to_string())
        );
        assert_eq!(
            Expression::neg(Expression::Variable("x")).to_string(),
            "-x".to_string()
        );
    }

    #[test]
    fn test_negation_display() {
        let (x, y) = (Expression::Variable("x"), Expression::Variable("y"));
        let z = &x - &y;
        assert_eq!(z.to_string(), "(x - y)");

        let z = x - (y * Expression::Integer(2));
        assert_eq!(z.to_string(), "(x - (y * 2))");
    }

    #[test]
    fn test_basic_simplification_on_init() {
        let a = Expression::Integer(2) + Expression::Integer(3);
        assert_eq!(a.to_string(), "5");
    }

    #[test]
    fn test_division() {
        let (x, y) = (Expression::Variable("x"), Expression::Variable("y"));
        let div_expr = x / y;
        assert_eq!(div_expr.to_string(), "(x * (y)^-1)");

        let div_expr = div_expr.substitute(&[("x", 4)]);
        assert_eq!(div_expr.to_string(), "(4 * (y)^-1)");

        let div_expr = div_expr.substitute(&[("y", 2)]);
        assert_eq!(div_expr.to_string(), "(4 * (2)^-1)");
    }
}
