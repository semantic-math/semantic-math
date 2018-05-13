let str = "1 - 2 + 3 - 4";

let str = "a^2 * b^2 * c^2^x";

let tokens = Lexer.lex(str);

tokens |> Array.map(Lexer.tokenToString) |> Array.iter(Js.log);

let makeToken = (t, value) =>
  Lexer.{
    t,
    value,
    loc: {
      start: (-1),
      end_: (-1),
    },
  };

let consume = () =>
  Lexer.(
    switch (Js.Array.shift(tokens)) {
    | Some(token) => token
    | None => makeToken(EOF, "")
    }
  );

let peek = () =>
  Lexer.(
    try (tokens[0]) {
    | Invalid_argument("index out of bounds") => makeToken(EOF, "")
    }
  );

type operator =
  | Add
  | Mul
  | Div
  | Exp
  | Neg
  | Pos;

let getOpPrecedence = op =>
  switch (op) {
  | Add => 3
  | Mul => 4
  /***
   * We allow division to have higher precedence so that it's easy to write
   * multiplication of fractions
   */
  | Div => 5
  | Neg => 6
  | Pos => 6
  | Exp => 7
  };

let getPrecedence = () =>
  switch (peek().t) {
  | PLUS => getOpPrecedence(Add)
  | MINUS => getOpPrecedence(Add)
  | STAR => getOpPrecedence(Mul)
  | CARET => getOpPrecedence(Exp)
  | SLASH => getOpPrecedence(Div)
  | _ => 0
  };

type node =
  | Apply(operator, list(node))
  | Identifier(string)
  | Number(string);

exception Unhandled;

let rec parseExpression = (precedence: int) : node => {
  let left = ref(parsePrefix());
  while (precedence < getPrecedence()) {
    left := parseInfix(left^, peek());
  };
  let result = left^;
  result;
}
and parseInfix = (left, token) =>
  Lexer.(
    switch (token.t) {
    | PLUS => Apply(Add, [left] @ parseNaryArgs(Add, token))
    /***
     * Parse minus as addition, parseNaryArgs converts any minus signs
     * to neg(ation) operators.
     */
    | MINUS => Apply(Add, [left] @ parseNaryArgs(Add, token))
    | STAR => Apply(Mul, [left] @ parseNaryArgs(Mul, token))
    | CARET =>
      consume() |> ignore;
      Apply(Exp, [left, parseExpression(getOpPrecedence(Exp))]);
    | SLASH =>
      consume() |> ignore;
      Apply(Div, [left, parseExpression(getOpPrecedence(Div))]);
    | _ => left
    }
  )
and parseNaryArgs = (op, token) : list(node) => {
  open Lexer;
  consume() |> ignore;
  let result = parseExpression(getOpPrecedence(op));
  switch (token.t, peek().t) {
  | (PLUS, PLUS | MINUS) => [result] @ parseNaryArgs(op, peek())
  | (MINUS, PLUS | MINUS) =>
    [Apply(Neg, [result])] @ parseNaryArgs(op, peek())
  | (a, b) when a == b => [result] @ parseNaryArgs(op, peek())
  | (MINUS, _) => [Apply(Neg, [result])]
  | (_, _) => [result]
  };
}
and parsePrefix = () : node =>
  Lexer.(
    switch (consume().t) {
    | MINUS => Apply(Neg, [parseExpression(getOpPrecedence(Neg))])
    | IDENTIFIER(name) => Identifier(name)
    | NUMBER(value) => Number(value)
    | _ => raise(Unhandled)
    }
  );

let result = parseExpression(0);

let opToString = op =>
  switch (op) {
  | Add => "+"
  | Mul => "*"
  | Neg => "neg"
  | Pos => "pos"
  | Div => "/"
  | Exp => "^"
  };

let rec nodeToString = node =>
  switch (node) {
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
  };

Js.log(nodeToString(result));
