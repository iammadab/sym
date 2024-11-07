use crate::Atom;
use crate::Expression;
use std::ops::{Add, Mul, Neg, Sub};
#[macro_export]
macro_rules! impl_atom_add {
    ($lhs:ty, $rhs:ty) => {
        impl Add<$rhs> for $lhs {
            type Output = Expression;

            fn add(self, rhs: $rhs) -> Self::Output {
                Expression::Add(Box::new(self.into()), Box::new(rhs.into()))
            }
        }
    };
}

#[macro_export]
macro_rules! impl_atom_sub {
    ($lhs:ty, $rhs:ty) => {
        impl Sub<$rhs> for $lhs {
            type Output = Expression;

            fn sub(self, rhs: $rhs) -> Self::Output {
                Expression::Add(
                    Box::new(self.into()),
                    Box::new(Expression::Neg(Box::new(rhs.into()))),
                )
            }
        }
    };
}

#[macro_export]
macro_rules! impl_atom_mul {
    ($lhs:ty, $rhs:ty) => {
        impl Mul<$rhs> for $lhs {
            type Output = Expression;

            fn mul(self, rhs: $rhs) -> Self::Output {
                Expression::Mul(Box::new(self.into()), Box::new(rhs.into()))
            }
        }
    };
}

#[macro_export]
macro_rules! impl_type_combination {
    ($impl_name:ident, $type_name:ty) => {
        $impl_name!($type_name, $type_name);
        $impl_name!(&$type_name, &$type_name);
        $impl_name!($type_name, &$type_name);
        $impl_name!(&$type_name, $type_name);
    };
}

impl_type_combination!(impl_atom_add, Atom);
impl_type_combination!(impl_atom_sub, Atom);
impl_type_combination!(impl_atom_mul, Atom);
