use std::fmt::{Display, Formatter, Write};
use std::ops::{Add, Mul, Sub};

#[derive(Clone, Debug)]
enum Atom {
    Variable(&'static str),
    Integer(isize),
}

impl Atom {
    fn evaluate(&self, substitution_map: &[(&'static str, isize)]) -> isize {
        match self {
            Atom::Integer(val) => *val,
            Atom::Variable(variable_name) => {
                for (var, val) in substitution_map {
                    if var == variable_name {
                        return *val;
                    }
                }
                panic!("didn't assign a concrete value to all variables");
            }
        }
    }

    fn substitute(&self, substitution_map: &[(&'static str, isize)]) -> Self {
        match self {
            Atom::Variable(variable_name) => {
                for (var, val) in substitution_map {
                    if var == variable_name {
                        return Atom::Integer(*val);
                    }
                }
                self.clone()
            }
            Atom::Integer(_) => self.clone(),
        }
    }
}

impl Display for Atom {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Atom::Variable(var_name) => f.write_str(var_name),
            Atom::Integer(val) => f.write_str(val.to_string().as_str()),
        }
    }
}

impl From<Atom> for Expression {
    fn from(value: Atom) -> Expression {
        Expression::Atom(value)
    }
}

impl From<&Atom> for Expression {
    fn from(value: &Atom) -> Self {
        value.clone().into()
    }
}

impl Add for &Atom {
    type Output = Expression;

    fn add(self, rhs: Self) -> Self::Output {
        Expression::Add(Box::new(self.into()), Box::new(rhs.into()))
    }
}

impl Sub for &Atom {
    type Output = Expression;

    fn sub(self, rhs: Self) -> Self::Output {
        Expression::Add(
            Box::new(self.into()),
            Box::new(Expression::Neg(Box::new(rhs.into()))),
        )
    }
}

impl Mul for &Atom {
    type Output = Expression;

    fn mul(self, rhs: Self) -> Self::Output {
        Expression::Mul(Box::new(self.into()), Box::new(rhs.into()))
    }
}

#[derive(Clone, Debug)]
enum Expression {
    Atom(Atom),
    Neg(Box<Expression>),
    Add(Box<Expression>, Box<Expression>),
    Mul(Box<Expression>, Box<Expression>),
}

impl Expression {
    fn neg(expression: Self) -> Self {
        Expression::Neg(Box::new(expression))
    }

    fn add(left: Self, right: Self) -> Self {
        Expression::Add(Box::new(left), Box::new(right))
    }

    fn mul(left: Self, right: Self) -> Self {
        Expression::Mul(Box::new(left), Box::new(right))
    }

    fn evaluate(&self, substitution_map: &[(&'static str, isize)]) -> isize {
        match self {
            Expression::Atom(atom) => atom.evaluate(substitution_map),
            Expression::Neg(expr) => -1 * expr.evaluate(substitution_map),
            Expression::Add(left, right) => {
                left.evaluate(substitution_map) + right.evaluate(substitution_map)
            }
            Expression::Mul(left, right) => {
                left.evaluate(substitution_map) * right.evaluate(substitution_map)
            }
        }
    }

    fn substitute(&self, substitution_map: &[(&'static str, isize)]) -> Self {
        match self {
            Expression::Atom(atom) => atom.substitute(substitution_map).into(),
            Expression::Neg(expr) => Expression::Neg(Box::new(expr.substitute(substitution_map))),
            Expression::Add(left, right) => Expression::Add(
                Box::new(left.substitute(substitution_map)),
                Box::new(right.substitute(substitution_map)),
            ),
            Expression::Mul(left, right) => Expression::Mul(
                Box::new(left.substitute(substitution_map)),
                Box::new(right.substitute(substitution_map)),
            ),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Atom(atom) => atom.fmt(f),
            Expression::Neg(expr) => {
                f.write_str("-")?;
                expr.fmt(f)
            }
            Expression::Add(left, right) => {
                f.write_str("(")?;
                left.fmt(f)?;
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

impl Add for &Expression {
    type Output = Expression;

    fn add(self, rhs: Self) -> Self::Output {
        Expression::Add(Box::new(self.clone()), Box::new(rhs.clone()))
    }
}

impl Add<&Atom> for &Expression {
    type Output = Expression;

    fn add(self, rhs: &Atom) -> Self::Output {
        Expression::Add(Box::new(self.clone()), Box::new(rhs.into()))
    }
}

impl Sub for &Expression {
    type Output = Expression;

    fn sub(self, rhs: Self) -> Self::Output {
        Expression::Add(
            Box::new(self.clone()),
            Box::new(Expression::Neg(Box::new(rhs.clone()))),
        )
    }
}

impl Sub<&Atom> for &Expression {
    type Output = Expression;

    fn sub(self, rhs: &Atom) -> Self::Output {
        Expression::Add(
            Box::new(self.clone()),
            Box::new(Expression::Neg(Box::new(rhs.into()))),
        )
    }
}

impl Mul for &Expression {
    type Output = Expression;

    fn mul(self, rhs: Self) -> Self::Output {
        Expression::Mul(Box::new(self.clone()), Box::new(rhs.clone()))
    }
}

impl Mul<&Atom> for &Expression {
    type Output = Expression;

    fn mul(self, rhs: &Atom) -> Self::Output {
        Expression::Mul(Box::new(self.clone()), Box::new(rhs.into()))
    }
}

#[cfg(test)]
mod tests {
    use crate::Atom;

    #[test]
    fn fake_test() {
        let (x, y) = (Atom::Variable("x"), Atom::Variable("y"));
        // 2x + 3y
        let expr = &(&x * &Atom::Integer(2)) + &(&y * &Atom::Integer(3));
        println!("{}", expr);
        let expr = expr.substitute(&[("x", 5)]);
        println!("{}", expr);
    }
}
