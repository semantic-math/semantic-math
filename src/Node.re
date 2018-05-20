/**
 * node and operator type definitions and operator precedence
 */
type node =
  | Apply(operator, list(node))
  | Identifier(string)
  | Number(string)
  | Ellipses
and operator =
  | Eq
  | Lt
  | Gt
  | Lte
  | Gte
  | Add
  | Sub
  | Mul([ | `Implicit | `Explicit])
  | Div
  | Exp
  | Subscript
  | Neg
  | Pos
  | Comma
  | Fact
  | Prime
  | Func(node);

let getOpPrecedence = op =>
  switch (op) {
  | Comma => 1
  | Eq => 2
  | Lt => 2
  | Gt => 2
  | Lte => 2
  | Gte => 2
  | Add => 3
  | Sub => 4
  | Mul(`Explicit) => 5
  /***
   * We allow division to have higher precedence so that it's easy to write
   * multiplication of fractions e.g. x/y * a/b should parse as [* [/ x y] [/ a b]]
   */
  | Div => 6
  /***
   * We give implicit multiplication higher precedence than division to support
   * parsing expressions like ab / cd as [/ [* a b] [* c d]].
   */
  | Mul(`Implicit) => 7
  | Neg => 8
  | Pos => 8
  | Fact => 9
  | Prime => 9
  | Exp => 10
  | Subscript => 10
  | Func(_) => 11
  };

let makeApply = (op, children) => Apply(op, children);
