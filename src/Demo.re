Js.log("re-math-parser demo");

let str = "-a+-+b++c";

let tokens = Lexer.lex(str);

tokens |> Array.map(Lexer.tokenToString) |> Array.iter(Js.log);

type operator =
  | Add
  | Mul
  | Neg
  | Pos;

let opToString = op =>
  switch (op) {
  | Add => "add"
  | Mul => "mul"
  | Neg => "neg"
  | Pos => "pos"
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
  parse: unit => node,
  precedence: int,
};

type infix_parselet = {
  parse2: node => node,
  precedence: int,
};

let rec getPrefixParselet = token =>
  Lexer.(
    switch (token.t) {
    | IDENTIFIER(name) =>
      Some({parse: () => Identifier(name), precedence: 0});
    | MINUS =>
      Some({parse: () => Apply(Neg, [parse(100)]), precedence: 100})
    | PLUS =>
      Some({parse: () => Apply(Pos, [parse(100)]), precedence: 100})
    | _ => None
    }
  )
and getInfixParselet = token =>
  Lexer.(
    switch (token.t) {
    | PLUS =>
      Some({parse2: left => Apply(Add, [left, parse(5)]), precedence: 5})
    | STAR =>
      Some({parse2: left => Apply(Mul, [left, parse(10)]), precedence: 10})
    | _ => None
    }
  )
and parse = (precedence: int) =>
  switch (consume()) {
  | Some(token) =>
    let left =
      switch (getPrefixParselet(token)) {
      | Some(parselet) => parselet.parse()
      | None => raise(Error)
      };
    switch (parseInfixNary(precedence, peek())) {
    | [] => left
    | otherArgs => Apply(Add, [left] @ otherArgs)
    };
  | None => raise(Error)
  }
and parseInfixNary = (precedence, token) : list(node) =>
  Lexer.(
    if (precedence < getPrecedence()) {
      /* since it's the right precedence we can consume the operator token */
      consume() |> ignore;
      let result = parse(5);  /* TODO: look up the precedence of the token */
      if (token.t == peek().t) {
        [result] @ parseInfixNary(precedence, token);
      } else {
        [result];
      }
    } else {
      [];
    }
  )
and parseInfix = (precedence, left) : node =>
  if (precedence < getPrecedence()) {
    switch (consume()) {
    | Some(token) =>
      switch (getInfixParselet(token)) {
      | Some(parselet) => parseInfix(precedence, parselet.parse2(left))
      | None => raise(Error)
      }
    | None => left
    };
  } else {
    left;
  }
and getPrecedence = () =>
  try (
    switch (getInfixParselet(peek())) {
    | Some(parselet) => parselet.precedence
    | None => 0
    }
  ) {
  | Invalid_argument("index out of bounds") => 0
  };

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