use crate::Expression;
use std::collections::HashMap;

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

    // compound-rewrite
    // collecting liketerms
    //

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

fn coefficient_expression_split(expr: Expression) -> (isize, Expression) {
    // it is assumed that the expression has already been simplified
    match expr {
        Expression::Neg(inner) => (-1, *inner.clone()),
        Expression::Mul(ref exprs) => {
            // since this should already be simplified
            // we assume that there should be only one integer expression
            // if none exists then the count is 1
            let mut terms = exprs.clone();

            let mut count = 1;
            if let Some(pos) = terms
                .iter()
                .position(|expr| matches!(expr, Expression::Integer(_)))
            {
                if let Expression::Integer(val) = terms[pos] {
                    count = val;
                }
                terms.remove(pos);
            }

            (count, Expression::Mul(terms))
        }
        _ => (1, expr),
    }
}

fn search_and_update_count(store: &mut Vec<(isize, Expression)>, count: isize, expr: Expression) {
    for (prev_count, matching_expr) in &mut *store {
        if expr == *matching_expr {
            *prev_count += count;
            return;
        }
    }

    store.push((count, expr));
}

#[cfg(test)]
mod tests {
    use crate::simplify::add::{coefficient_expression_split, search_and_update_count};
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

    #[test]
    fn test_coefficient_split() {
        assert_eq!(
            coefficient_expression_split(Expression::Variable("x".to_string())),
            (1, Expression::Variable("x".to_string()))
        );
        assert_eq!(
            coefficient_expression_split(Expression::Mul(vec![
                Expression::Variable("x".to_string()),
                Expression::Variable("y".to_string())
            ])),
            (
                1,
                Expression::Mul(vec![
                    Expression::Variable("y".to_string()),
                    Expression::Variable("x".to_string())
                ])
            )
        );
        assert_eq!(
            coefficient_expression_split(Expression::Mul(vec![
                Expression::Variable("x".to_string()),
                Expression::Integer(3),
                Expression::Variable("y".to_string())
            ])),
            (
                3,
                Expression::Mul(vec![
                    Expression::Variable("y".to_string()),
                    Expression::Variable("x".to_string())
                ])
            )
        );
    }

    #[test]
    fn test_search_and_update() {
        let mut result = vec![];
        search_and_update_count(&mut result, 2, Expression::Integer(3));
        assert_eq!(result.len(), 1);
        search_and_update_count(
            &mut result,
            -1,
            Expression::Add(vec![
                Expression::Integer(2),
                Expression::Variable("x".to_string()),
            ]),
        );
        assert_eq!(result.len(), 2);
        search_and_update_count(
            &mut result,
            1,
            Expression::Add(vec![
                Expression::Variable("x".to_string()),
                Expression::Integer(2),
            ]),
        );
        assert_eq!(result.len(), 2);
        assert_eq!(result[1].0, 0);
    }
}
