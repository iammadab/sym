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
