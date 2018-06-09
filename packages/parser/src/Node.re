/**
 * node and operator type definitions and operator precedence
 */
type t =
  | Apply(operator, list(t))
  | Identifier(string)
  | Number(string)
  | Ellipses
and operator =
  | Nul
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
  | Func(t);

let getOpPrecedence = op =>
  switch (op) {
  | Nul => 0
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

let rec toString = node =>
  switch (node) {
  | Apply(op, children) =>
    "["
    ++ Js.Array.joinWith(
         " ",
         Array.of_list([
           opToString(op),
           ...List.map(toString, children),
         ]),
       )
    ++ "]"
  | Identifier(name) => name
  | Number(value) => value
  | Ellipses => "..."
  }
and opToString = op =>
  switch (op) {
  | Nul => "nul"
  | Comma => ","
  | Eq => "="
  | Lt => "<"
  | Gt => ">"
  | Lte => "<="
  | Gte => ">="
  | Add => "+"
  | Sub => "-"
  | Mul(_) => "*"
  | Neg => "neg"
  | Pos => "pos"
  | Div => "/"
  | Exp => "^"
  | Subscript => "_"
  | Fact => "!"
  | Prime => "'"
  | Func(name) => toString(name)
  };

let rec toJson = node =>
  Json.Encode.(
    switch (node) {
    | Apply(op, args) =>
      object_([
        ("type", string("Apply")),
        ("op", opToJson(op)),
        ("args", jsonArray(Array.map(toJson, Array.of_list(args)))),
      ])
    | Number(value) =>
      object_([("type", string("Number")), ("value", string(value))])
    | Identifier(name) =>
      object_([("type", string("Identifier")), ("name", string(name))])
    | Ellipses => object_([("type", string("Ellipses"))])
    }
  )
and opToJson = op : Js.Json.t =>
  Json.Encode.(
    switch (op) {
    | Nul => string("nul")
    | Comma => string("comma")
    | Eq => string("eq")
    | Lt => string("lt")
    | Gt => string("gt")
    | Lte => string("lte")
    | Gte => string("gte")
    | Add => string("add")
    | Sub => string("sub")
    | Mul(_) => string("mul")
    | Neg => string("neg")
    | Pos => string("pos")
    | Div => string("div")
    | Exp => string("exp")
    | Subscript => string("subscript")
    | Fact => string("fact")
    | Prime => string("prime")
    | Func(name) => toJson(name)
    }
  );