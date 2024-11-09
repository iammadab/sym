use crate::Expression;
use std::ops::{Add, Div, Mul, Neg, Sub};
#[macro_export]
macro_rules! impl_add {
    ($lhs:ty, $rhs:ty) => {
        impl Add<$rhs> for $lhs {
            type Output = Expression;

            fn add(self, rhs: $rhs) -> Self::Output {
                Expression::add(self.clone(), rhs.clone())
            }
        }
    };
}

#[macro_export]
macro_rules! impl_sub {
    ($lhs:ty, $rhs:ty) => {
        impl Sub<$rhs> for $lhs {
            type Output = Expression;

            fn sub(self, rhs: $rhs) -> Self::Output {
                Expression::add(self.clone(), Expression::neg(rhs.clone()))
            }
        }
    };
}

#[macro_export]
macro_rules! impl_mul {
    ($lhs:ty, $rhs:ty) => {
        impl Mul<$rhs> for $lhs {
            type Output = Expression;

            fn mul(self, rhs: $rhs) -> Self::Output {
                Expression::mul(self.clone(), rhs.clone())
            }
        }
    };
}

#[macro_export]
macro_rules! impl_div {
    ($lhs:ty, $rhs:ty) => {
        impl Div<$rhs> for $lhs {
            type Output = Expression;

            fn div(self, rhs: $rhs) -> Self::Output {
                Expression::mul(self.clone(), Expression::inv(rhs.clone()))
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

// Expression arithmetic operations
impl_type_combination!(impl_add, Expression);
impl_type_combination!(impl_sub, Expression);
impl_type_combination!(impl_mul, Expression);
impl_type_combination!(impl_div, Expression);
