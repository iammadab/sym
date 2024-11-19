## Symbolic Manipulation Library (WIP)

Algorithmically manipulate mathematical expressions.

#### Supported Expressions
- Variables e.g. "x", "y"
- Fractions e.g $\frac{2}{3}, \frac{5}{1}$
- Neg e.g. -2, -x, -expr
- Inv e.g $x^{-1}$, $expr^{-1}$
- Exponentiation e.g $2^3$, $expr^n$
- Addition e.g. $a + b + c$ or $a + b + Neg(c) = a + b - c$ 
- Multiplication e.g. $a * b * c$ or $a * b * Inv(c) = \frac{a*b}{c}$

#### Example
```rust
let (x, y) = (Symbol::Variable("x".to_string()), Symbol::Variable("y".to_string()));
let z = Symbol::Variable("z".to_string())

let expr = (x + y) * z; // (x + y)z
dbg!(expr.simplify()) // xz + yz
```
