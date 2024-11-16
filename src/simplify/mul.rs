use crate::Expression;

pub(crate) fn simplify_mul(expression: Expression) -> Expression {
    let children = expression.children();

    // Substitution Rules
    // 1. (a * b) * (c * d) = a * b * c * d
    // 2. Int(x) * Int(y) = Int(x * y)

    // Sub 1.
    // check if we have any multiplication node in the operand
    // if a multiplication operand exists, merge the operands children with current node
    let mut flat_nodes = children.into_iter().flat_map(|child| match child {
        Expression::Mul(_) => child.children(),
        _ => vec![child],
    });

    // compute the product of integers in the expression
    // while removing the individual integer terms from the list
    let mut prod = 1;
    let mut non_integer_nodes = flat_nodes
        .into_iter()
        .filter(|child| match child {
            Expression::Integer(val) => {
                prod *= val;
                false
            }
            _ => true,
        })
        .collect::<Vec<_>>();

    let prod_node = Expression::Integer(prod);

    if non_integer_nodes.is_empty() || prod == 0 {
        return prod_node;
    }

    if prod == 1 {
        return Expression::Mul(non_integer_nodes);
    }

    // prod_node is a non-zero int
    Expression::Mul(vec![vec![prod_node], non_integer_nodes].concat())
}

#[cfg(test)]
mod tests {
    use crate::Expression;

    #[test]
    fn test_mul_simplification() {
        // Integers mixed with variables
        assert_eq!(
            Expression::Mul(vec![
                Expression::Integer(3),
                Expression::Variable("x".to_string()),
                Expression::Integer(4),
                Expression::Variable("y".to_string())
            ])
                .simplify()
                .to_string(),
            "12xy"
        );

        assert_eq!(
            Expression::Mul(vec![
                Expression::Mul(vec![
                    Expression::Variable("a".to_string()),
                    Expression::Integer(2)
                ]),
                Expression::Mul(vec![
                    Expression::Integer(-2),
                    Expression::Variable("b".to_string()),
                    Expression::Integer(2)
                ]),
                Expression::Mul(vec![
                    Expression::Integer(2),
                    Expression::Variable("c".to_string()),
                    Expression::Integer(2)
                ]),
            ])
                .simplify()
                .to_string(),
            "-32abc"
        );
    }
}
