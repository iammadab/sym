use crate::Atom;
use crate::Expression;
use std::ops::{Add, Mul, Neg, Sub};
#[macro_export]
macro_rules! impl_atom_add {
    ($lhs:ty, $rhs:ty, $conversion:ident) => {
        impl Add<$rhs> for $lhs {
            type Output = Expression;

            fn add(self, rhs: $rhs) -> Self::Output {
                Expression::Add(Box::new(self.$conversion()), Box::new(rhs.$conversion()))
            }
        }
    };
}

#[macro_export]
macro_rules! impl_atom_sub {
    ($lhs:ty, $rhs:ty, $conversion:ident) => {
        impl Sub<$rhs> for $lhs {
            type Output = Expression;

            fn sub(self, rhs: $rhs) -> Self::Output {
                Expression::Add(
                    Box::new(self.$conversion()),
                    Box::new(Expression::Neg(Box::new(rhs.$conversion()))),
                )
            }
        }
    };
}

#[macro_export]
macro_rules! impl_atom_mul {
    ($lhs:ty, $rhs:ty, $conversion:ident) => {
        impl Mul<$rhs> for $lhs {
            type Output = Expression;

            fn mul(self, rhs: $rhs) -> Self::Output {
                Expression::Mul(Box::new(self.$conversion()), Box::new(rhs.$conversion()))
            }
        }
    };
}

#[macro_export]
macro_rules! impl_type_combination {
    ($impl_name:ident, $type_name:ty, $conversion:ident) => {
        $impl_name!($type_name, $type_name, $conversion);
        $impl_name!(&$type_name, &$type_name, $conversion);
        $impl_name!($type_name, &$type_name, $conversion);
        $impl_name!(&$type_name, $type_name, $conversion);
    };
}

/// Atom arithmetic operations
impl_type_combination!(impl_atom_add, Atom, into);
impl_type_combination!(impl_atom_sub, Atom, into);
impl_type_combination!(impl_atom_mul, Atom, into);

/// Expression arithmetic operations
impl_type_combination!(impl_atom_add, Expression, clone);
impl_type_combination!(impl_atom_sub, Expression, clone);
impl_type_combination!(impl_atom_mul, Expression, clone);
