use crate::Expression;

pub fn simplify_neg(expression: Expression) -> Expression {
    // Substitution Rules
    // 1. Neg(Neg(x)) => x
    // 2. Neg(integer) => -1 * integer

    // match expression {
    //     Expression::Integer(val) => Expression::Integer(-1 * val),
    //     Expression::Variable()
    // }

    todo!()
}

pub fn simplify_inv(expression: Expression) -> Expression {
    todo!()
}
pub fn simplify_add(expression: Expression) -> Expression {
    todo!()
}

pub fn simplify_mul(expression: Expression) -> Expression {
    todo!()
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_negation_simplification() {
        todo!()
    }
}
