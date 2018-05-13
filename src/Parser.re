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

type node =
  | Apply(operator, list(node))
  | Identifier(string)
  | Number(string);

exception Unhandled;

let makeToken = (t, value) =>
  Lexer.{
    t,
    value,
    loc: {
      start: (-1),
      end_: (-1),
    },
  };

let parse = tokens : node => {
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
  let getPrecedence = () =>
    Lexer.(
      switch (peek().t) {
      | PLUS => getOpPrecedence(Add)
      | MINUS => getOpPrecedence(Add)
      | STAR => getOpPrecedence(Mul)
      | IDENTIFIER(_) => getOpPrecedence(Mul)
      | LEFT_PAREN => getOpPrecedence(Mul)
      | CARET => getOpPrecedence(Exp)
      | SLASH => getOpPrecedence(Div)
      | _ => 0
      }
    );
  let splitIdentifier = name =>
    Array.iter(
      letter =>
        /* TODO: provide correct location info for letter tokens */
        Js.Array.unshift(makeToken(IDENTIFIER(letter), letter), tokens)
        |> ignore,
      Js.Array.reverseInPlace(Js.String.split("", name)),
    );
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
      | LEFT_PAREN => Apply(Mul, [left] @ parseNaryArgs(Mul, peek()));
      | IDENTIFIER(name) =>
        consume() |> ignore; /* consume the un-split identifier */
        splitIdentifier(name);
        Apply(Mul, [left] @ parseNaryArgs(Mul, peek()));
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
    let result =
      switch (token.t) {
      | IDENTIFIER(_) => parseExpression(getOpPrecedence(op))
      | _ =>
        consume() |> ignore;
        parseExpression(getOpPrecedence(op));
      };
    switch (token.t, peek().t) {
    | (PLUS, PLUS | MINUS) => [result] @ parseNaryArgs(op, peek())
    | (MINUS, PLUS | MINUS) =>
      [Apply(Neg, [result])] @ parseNaryArgs(op, peek())
    | (NUMBER(_) | IDENTIFIER(_), IDENTIFIER(_) | LEFT_PAREN) =>
      [result] @ parseNaryArgs(op, peek())
    | (a, b) when a == b => [result] @ parseNaryArgs(op, peek())
    | (MINUS, _) => [Apply(Neg, [result])]
    | (_, _) => [result]
    };
  }
  and parsePrefix = () : node =>
    Lexer.(
      switch (consume().t) {
      | MINUS => Apply(Neg, [parseExpression(getOpPrecedence(Neg))])
      | IDENTIFIER(name) =>
        if (String.length(name) > 1) {
          splitIdentifier(name);
          switch (consume().t) {
          | IDENTIFIER(letter) => parseInfix(Identifier(letter), peek())
          | _ => raise(Unhandled)
          };
        } else {
          Identifier(name);
        };
      | NUMBER(value) => Number(value)
      | LEFT_PAREN => 
        let children = parseMulByParens();
        switch (List.length(children)) {
        | 0 => raise(Unhandled)
        | 1 => List.hd(children)
        | _ => Apply(Mul, children)
        };
      | _ => raise(Unhandled)
      }
    )
  and parseMulByParens = () => {
    let expr = parseExpression(0);
    switch (consume().t, peek().t) {
    | (RIGHT_PAREN, LEFT_PAREN) =>
      consume() |> ignore;
      [expr] @ parseMulByParens();
    | _ => [expr]
    };
  };
  parseExpression(0);
};

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