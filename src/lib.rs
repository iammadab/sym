use std::ops::{Add, Mul, Sub};

#[derive(Clone, Debug)]
enum Atom {
    Variable(&'static str),
    Integer(usize),
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

impl Add for Atom {
    type Output = Expression;

    fn add(self, rhs: Self) -> Self::Output {
        Expression::Add(Box::new(self.into()), Box::new(rhs.into()))
    }
}

impl Sub for Atom {
    type Output = Expression;

    fn sub(self, rhs: Self) -> Self::Output {
        Expression::Add(
            Box::new(self.into()),
            Box::new(Expression::Neg(Box::new(rhs.into()))),
        )
    }
}

impl Mul for Atom {
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
    fn neg(&self) -> Expression {
        Expression::Neg(Box::new(self.clone()))
    }
}

impl Add for Expression {
    type Output = Expression;

    fn add(self, rhs: Self) -> Self::Output {
        Expression::Add(Box::new(self), Box::new(rhs))
    }
}

impl Add<Atom> for Expression {
    type Output = Expression;

    fn add(self, rhs: Atom) -> Self::Output {
        Expression::Add(Box::new(self), Box::new(rhs.into()))
    }
}

impl Sub for Expression {
    type Output = Expression;

    fn sub(self, rhs: Self) -> Self::Output {
        Expression::Add(Box::new(self), Box::new(Expression::Neg(Box::new(rhs))))
    }
}

impl Sub<Atom> for Expression {
    type Output = Expression;

    fn sub(self, rhs: Atom) -> Self::Output {
        Expression::Add(
            Box::new(self),
            Box::new(Expression::Neg(Box::new(rhs.into()))),
        )
    }
}

impl Mul for Expression {
    type Output = Expression;

    fn mul(self, rhs: Self) -> Self::Output {
        Expression::Mul(Box::new(self), Box::new(rhs))
    }
}

impl Mul<Atom> for Expression {
    type Output = Expression;

    fn mul(self, rhs: Atom) -> Self::Output {
        Expression::Mul(Box::new(self), Box::new(rhs.into()))
    }
}

#[cfg(test)]
mod tests {
    use crate::Atom;

    #[test]
    fn fake_test() {
        let x = Atom::Variable("x");
        let y = x + Atom::Integer(1);
        let z = y * Atom::Integer(2);
        let m = z - Atom::Integer(3);
        dbg!(m);
    }
}
