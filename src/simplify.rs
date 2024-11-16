use crate::Expression;
pub(crate) fn simplify_neg(expression: Expression) -> Expression {
    let neg_inner = expression.children().pop().unwrap();

    // Substitution Rules
    // 1. Neg(Neg(x)) => x
    // 2. Neg(integer) => -1 * integer
    // 3. Neg(a + b + c) => Neg(a) + Neg(b) + Neg(c)
    match neg_inner {
        Expression::Neg(inner_expr) => (*inner_expr).clone(),
        Expression::Integer(val) => Expression::Integer(-1 * val),
        Expression::Add(_) => {
            let add_terms = neg_inner.children();
            Expression::Add(
                add_terms
                    .into_iter()
                    .map(|t| Expression::Neg(Box::new(t)))
                    .collect(),
            )
            .simplify()
        }
        _ => Expression::Neg(Box::new(neg_inner)),
    }
}

pub(crate) fn simplify_inv(expression: Expression) -> Expression {
    let child = expression.children_ref()[0];

    // Substitution Rules
    // Inv(Inv(x)) => x
    match child {
        Expression::Inv(inner_expr) => (**inner_expr).clone(),
        _ => expression,
    }
}

pub(crate) fn simplify_exp(expression: Expression) -> Expression {
    expression
}

pub(crate) fn simplify_add(expression: Expression) -> Expression {
    let children = expression.children();

    // Substitution Rules
    // 1. (a + b) + (c + d) = a + b + c + d
    // 2. Int(x) + Int(y) = Int(x + y)

    // Sub 1.
    // check if we have any addition node in the operand
    // if an addition operand exists, merge the operands children with current node
    let mut flat_nodes = children.into_iter().flat_map(|child| match child {
        Expression::Add(_) => child.children(),
        _ => vec![child],
    });

    // Sub 2.
    // compute the sum of integers in the expression
    // while removing the individual integer terms from the list
    let mut sum = 0;
    let mut non_integer_nodes = flat_nodes
        .filter(|child| match child {
            Expression::Integer(val) => {
                sum += val;
                false
            }
            _ => true,
        })
        .collect::<Vec<_>>();

    let sum_node = Expression::Integer(sum);

    if non_integer_nodes.is_empty() {
        return sum_node;
    }

    // push the sum node into the node list if it's not zero
    if sum != 0 {
        non_integer_nodes.push(sum_node);
    }

    Expression::Add(non_integer_nodes)
}

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
    fn test_negation_simplification() {
        // Neg(Neg(a)) = a
        assert_eq!(
            Expression::Neg(Box::new(Expression::Neg(Box::new(Expression::Variable(
                "a".to_string()
            )))))
            .simplify()
            .to_string(),
            "a"
        );

        // Neg(2) = -2
        assert_eq!(
            Expression::Neg(Box::new(Expression::Integer(2)))
                .simplify()
                .to_string(),
            "-2"
        );

        // Neg(-2) = 2
        assert_eq!(
            Expression::Neg(Box::new(Expression::Integer(-2)))
                .simplify()
                .to_string(),
            "2"
        );

        // Neg(Add(a, b, c)) = Add(Neg(a), Neg(b), Neg(c))
        let (a, b, c) = (
            Expression::Variable("a".to_string()),
            Expression::Variable("b".to_string()),
            Expression::Variable("c".to_string()),
        );
        let expr = -(&a + &b + &c);
        assert_eq!(expr.simplify(), -a - b - c);
    }

    #[test]
    fn test_inverse_simplification() {
        // Inv(Inv(a)) = a
        assert_eq!(
            Expression::Inv(Box::new(Expression::Inv(Box::new(Expression::Variable(
                "a".to_string()
            )))))
            .simplify()
            .to_string(),
            "a"
        );
    }

    #[test]
    fn test_add_simplification() {
        // 2 integers
        assert_eq!(
            Expression::Add(vec![Expression::Integer(2), Expression::Integer(3)])
                .simplify()
                .to_string(),
            "5"
        );

        // 4 integers
        assert_eq!(
            Expression::Add(vec![
                Expression::Integer(2),
                Expression::Integer(3),
                Expression::Integer(4),
                Expression::Integer(5)
            ])
            .simplify()
            .to_string(),
            "14"
        );

        // Integers mixed with variables
        assert_eq!(
            Expression::Add(vec![
                Expression::Integer(3),
                Expression::Variable("x".to_string()),
                Expression::Integer(4),
                Expression::Variable("y".to_string())
            ])
            .simplify()
            .to_string(),
            "(x + y + 7)"
        );

        assert_eq!(
            Expression::Add(vec![
                Expression::Add(vec![
                    Expression::Variable("a".to_string()),
                    Expression::Integer(2)
                ]),
                Expression::Add(vec![
                    Expression::Integer(-2),
                    Expression::Variable("b".to_string()),
                    Expression::Integer(2)
                ]),
                Expression::Add(vec![
                    Expression::Integer(2),
                    Expression::Variable("c".to_string()),
                    Expression::Integer(2)
                ]),
            ])
            .simplify()
            .to_string(),
            "(a + b + c + 6)"
        );
    }

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
