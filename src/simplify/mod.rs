mod add;
mod exp;
mod inv;
mod mul;
mod neg;
mod fraction;

pub(crate) use add::simplify_add;
pub(crate) use exp::simplify_exp;
pub(crate) use inv::simplify_inv;
pub(crate) use mul::simplify_mul;
pub(crate) use neg::simplify_neg;
pub(crate) use fraction::simplify_fraction;

fn gcd(mut a: isize, mut b: isize) -> isize {
    while b != 0 {
        let remainder = a % b;
        a = b;
        b = remainder;
    }
    a
}
