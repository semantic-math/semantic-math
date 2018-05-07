type node = {
  node_desc,
  loc: Lexer.location,
}
and node_desc =
  | Apply(operator, list(node))
  | Identifier(string)
  | Number(string)
and operator =
  | Add
  | Sub
  | Mul([ | `Explicit | `Implicit])
  | Div
  | Exp
  | Neg
  | Pos
  | Eq
  | Gt
  | Gte
  | Lt
  | Lte
  | Func(node)
  | LeftParen /* not a real operator */
  | RightParen /* not a real operator */
  | Comma;

exception NoOperands;

let makeApply = (op: operator, children: list(node)) => {
  let len = List.length(children);
  switch (List.hd(children), List.nth(children, len - 1)) {
  | (first, last) => {
      node_desc: Apply(op, children),
      loc: {
        Lexer.start: first.loc.Lexer.start,
        Lexer.end_: last.loc.Lexer.end_,
      },
    }
  | exception _ => raise(NoOperands)
  };
};

let makeNumber = (token: Lexer.token) => {
  /* TODO(kevinb): verify that it's a number token */
  node_desc: Number(token.Lexer.value),
  loc: token.Lexer.loc,
};

let makeIdentifier = (token: Lexer.token) => {
  /* TODO(kevinb): verify that it's an identifier token */
  node_desc: Identifier(token.Lexer.value),
  loc: token.Lexer.loc,
};

let makeNewIdentifier = (id: string, start: int, end_: int) => {
  node_desc: Identifier(id),
  loc: {
    Lexer.start,
    Lexer.end_,
  },
};

let updateLoc = (node: node, start: int, end_: int) => {
  node_desc: node.node_desc,
  loc: {
    Lexer.start,
    Lexer.end_,
  },
};

let rec nodeToString = node =>
  switch (node.node_desc) {
  | Apply(op, children) =>
    "["
    ++ Js.Array.joinWith(
         " ",
         Array.of_list([
           opToString(op),
           ...List.map(nodeToString, children),
         ]),
       )
    ++ "]"
  | Identifier(name) => name
  | Number(value) => value
  }
and opToString = (op: operator) =>
  switch (op) {
  | Add => "+"
  | Sub => "-"
  | Mul(_) => "*"
  | Div => "/"
  | Neg => "neg"
  | Pos => "pos"
  | Exp => "^"
  | Eq => "="
  | LeftParen => "("
  | RightParen => ")"
  | Gt => ">"
  | Gte => ">="
  | Lt => "<"
  | Lte => "<="
  | Func(node) => nodeToString(node)
  | Comma => ","
  };

let precedence = op =>
  switch (op) {
  | Comma => (-1)
  | Eq
  | Lt
  | Lte
  | Gt
  | Gte => 0
  | LeftParen
  | RightParen => 1
  | Add
  | Sub => 2
  | Mul(`Explicit) => 3
  | Mul(`Implicit)
  | Div => 4
  | Neg
  | Pos => 5
  | Exp => 6
  | Func(_) => 7
  };