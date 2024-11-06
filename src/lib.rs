use std::ops::{Add, Mul};

#[derive(Clone)]
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

impl Mul for Atom {
    type Output = Expression;

    fn mul(self, rhs: Self) -> Self::Output {
        Expression::Mul(Box::new(self.into()), Box::new(rhs.into()))
    }
}

#[derive(Clone)]
enum Expression {
    Atom(Atom),
    Add(Box<Expression>, Box<Expression>),
    Mul(Box<Expression>, Box<Expression>),
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
