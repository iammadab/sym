use std::collections::HashMap;
use crate::Expression;

pub(crate) fn simplify_add(expression: Expression) -> Expression {
    // TODO: document process

    // simplify each term in the add expression
    let terms = expression.children().into_iter().map(|c| c.simplify());

    // collapse terms that are additions
    let terms = terms.flat_map(|child| match child {
        Expression::Add(_) => child.children(),
        _ => vec![child],
    });

    // rewrite integers
    let mut sum = 0;
    let terms = terms
        .filter(|t| match t {
            Expression::Integer(val) => {
                sum += val;
                false
            }
            _ => true,
        })
        .collect::<Vec<_>>();

    // rewrite variables
    let mut variable_map: HashMap<String, isize> = HashMap::new();
    let terms = terms
        .into_iter()
        .filter(|t| match t {
            Expression::Variable(var_name) => {
                variable_map
                    .entry(var_name.to_string())
                    .and_modify(|v| *v += 1)
                    .or_insert(1);
                false
            }
            Expression::Neg(expr) => match &**expr {
                Expression::Variable(var_name) => {
                    variable_map
                        .entry(var_name.to_string())
                        .and_modify(|v| *v -= 1)
                        .or_insert(-1);
                    false
                }
                _ => true,
            },
            _ => true,
        })
        .collect::<Vec<_>>();

    // construct compound variables
    let mut variable_rewrite_terms = vec![];
    for (variable_name, count) in variable_map {
        if count == 1 {
            variable_rewrite_terms.push(Expression::Variable(variable_name));
            continue;
        }

        if count == -1 {
            variable_rewrite_terms.push(Expression::Neg(Box::new(Expression::Variable(
                variable_name,
            ))));
            continue;
        }

        variable_rewrite_terms.push(Expression::Mul(vec![
            Expression::Integer(count),
            Expression::Variable(variable_name),
        ]));
    }

    // compound rewrite
    let mut final_terms = vec![terms, variable_rewrite_terms];
    if sum != 0 {
        final_terms.push(vec![Expression::Integer(sum)]);
    }
    let mut final_terms = final_terms.concat();

    if final_terms.len() == 1 {
        return final_terms.pop().unwrap();
    }

    Expression::Add(final_terms)
}

#[cfg(test)]
mod tests {
    use crate::Expression;

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
}