Js.log("re-math-parser demo");

let str = "a + b * c * d + e ^ -f ^ g";
/* let str = "a ^ b ^ c"; */
let tokens = Lexer.lex(str);

tokens |> Array.map(Lexer.tokenToString) |> Array.iter(Js.log);

type operator =
  | Add
  | Mul
  | Neg
  | Pos
  | Div
  | Exp;

let opToString = op =>
  switch (op) {
  | Add => "add"
  | Mul => "mul"
  | Neg => "neg"
  | Pos => "pos"
  | Div => "div"
  | Exp => "exp"
  };

type node =
  | Apply(operator, list(node))
  | Identifier(string)
  | Number(string);

let peek = () =>
  Lexer.(
    try (tokens[0]) {
    | Invalid_argument("index out of bounds") => {
        t: EOF,
        value: "",
        loc: {
          start: (-1),
          end_: (-1),
        },
      }
    }
  );

let consume = () => Js.Array.shift(tokens);

exception Error;

type prefix_parselet = {
  parsePrefix: unit => node,
  precedence: int,
};

type infix_parselet = {
  parseBinary: node => node,
  precedence: int,
};

type nary_infix_parselet = {
  parseNary: (int, Lexer.token) => list(node),
  operator,
};

let rec getPrefixParselet = token =>
  Lexer.(
    switch (token.t) {
    | IDENTIFIER(name) =>
      Some({parsePrefix: () => Identifier(name), precedence: 0})
    | MINUS =>
      Some({parsePrefix: () => Apply(Neg, [parse(100)]), precedence: 100})
    | PLUS =>
      Some({parsePrefix: () => Apply(Pos, [parse(100)]), precedence: 100})
    | _ => None
    }
  )
and getInfixNaryParselet = token =>
  Lexer.(
    switch (token.t) {
    | PLUS =>
      Some({
        parseNary: parseInfixNary(10), /* precedence = 10 */
        operator: Add,
      })
    | STAR =>
      Some({
        parseNary: parseInfixNary(20), /* precedence = 20 */
        operator: Mul,
      })
    | _ => None
    }
  )
and getInfixBinaryParselet = token =>
  Lexer.(
    switch (token.t) {
    | SLASH =>
      Some({
        parseBinary: left => Apply(Div, [left, parse(20)]),
        precedence: 20,
      })
    | CARET =>
      Some({
        parseBinary: left => Apply(Exp, [left, parse(30)]),
        precedence: 30,
      })
    | _ => None
    }
  )
and parse = (precedence: int) =>
  switch (consume()) {
  | Some(token) =>
    let left =
      switch (getPrefixParselet(token)) {
      | Some(parselet) => parselet.parsePrefix()
      | None => raise(Error)
      };
    /* parse n-ary infix operators */
    switch (getInfixNaryParselet(peek())) {
    | Some(parselet) =>
      /* TODO: encapsulate this with in the parselet */
      switch (parselet.parseNary(precedence, peek())) {
      | [] => left
      | otherArgs => Apply(parselet.operator, [left] @ otherArgs)
      }
    | None => parseBinaryInfix(precedence, left)
    };
  | None => raise(Error)
  }
/* TODO: rename to parseNaryArgs */
and parseInfixNary = (opPrecendence, precedence, token) : list(node) =>
  Lexer.(
    if (precedence < getPrecedence()) {
      /* since it's the right precedence we can consume the operator token */
      consume() |> ignore;
      let result = parse(opPrecendence);
      token.t == peek().t ?
        [result] @ parseInfixNary(opPrecendence, precedence, token) :
        [result];
    } else {
      [];
    }
  )
and parseBinaryInfix = (precedence, left) : node =>
  if (precedence < getPrecedence()) {
    switch (consume()) {
    | Some(token) =>
      switch (getInfixBinaryParselet(token)) {
      | Some(parselet) =>
        parseBinaryInfix(precedence, parselet.parseBinary(left))
      | None => raise(Error)
      }
    | None => left
    };
  } else {
    left;
  }
and getPrecedence = () : int =>
  Lexer.(
    /* This is only necessary for handle infix operators */
    try (
      switch (peek().t) {
      | PLUS => 10
      | STAR => 20
      | SLASH => 20
      | CARET => 30
      | _ => 0
      }
    ) {
    | Invalid_argument("index out of bounds") => 0
    }
  );

let result = parse(0);

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
/* let ast = Parser.parse(tokens, str);
   Js.log(Node.nodeToString(ast));

   let result = Evaluate.evaluate(ast);
   Js.log(result); */