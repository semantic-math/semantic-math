type operator =
  | Add
  | Mul
  | Neg
  | Pos
  | Div
  | Exp;

let opToString = op =>
  switch (op) {
  | Add => "+"
  | Mul => "*"
  | Neg => "neg"
  | Pos => "pos"
  | Div => "/"
  | Exp => "^"
  };

let getOpPrecedence = op =>
  switch (op) {
  | Add => 10
  | Mul => 20
  | Div => 20
  | Exp => 30
  | Pos => 25
  | Neg => 25
  };

type node =
  | Apply(operator, list(node))
  | Identifier(string)
  | Number(string);

exception Error;

let makeToken = (t, value) =>
  Lexer.{
    t,
    value,
    loc: {
      start: (-1),
      end_: (-1),
    },
  };

let parse = (tokens, src: string) => {
  let peek = () =>
    Lexer.(
      try (tokens[0]) {
      | Invalid_argument("index out of bounds") => makeToken(EOF, "")
      }
    );
  let getPrecedence = () =>
    Lexer.(
      /* This is only necessary for handle infix operators */
      switch (peek().t) {
      | PLUS => 10
      | MINUS => 10
      | STAR => 20
      | SPACE => 25 /* used for implicit multiplication */
      | SLASH => 20
      | CARET => 30
      | _ => 0
      }
    );
  let consume = () =>
    Lexer.(
      switch (Js.Array.shift(tokens)) {
      | Some(token) => token
      | None => makeToken(EOF, "")
      }
    );
  let rec getPrefixParselet = token =>
    Lexer.(
      switch (token.t) {
      | IDENTIFIER(name) =>
        Some(
          (
            () =>
              if (String.length(name) > 1) {
                let letters = Js.String.split("", name);
                Array.iteri(
                  (i, letter) => {
                    if (i > 0) {
                      Js.Array.unshift(makeToken(SPACE, " "), tokens)
                      |> ignore;
                    };
                    Js.Array.unshift(
                      makeToken(IDENTIFIER(letter), letter),
                      tokens,
                    )
                    |> ignore;
                  },
                  Js.Array.reverseInPlace(letters),
                );
                switch (consume().t) {
                | IDENTIFIER(letter) =>
                  parseNaryInfix(Mul, 0, Identifier(letter))
                | _ =>
                  Js.log("before error");
                  raise(Error);
                };
              } else {
                Identifier(name);
              }
          ),
        )
      | NUMBER(value) => Some((() => Number(value)))
      | MINUS => Some(parsePrefix(Neg))
      | PLUS => Some(parsePrefix(Pos))
      | LEFT_PAREN =>
        Some(
          (
            () => {
              let expr = parseExpression(0);
              switch (consume().t) {
              | RIGHT_PAREN => expr
              | _ => raise(Error)
              };
            }
          ),
        )
      | _ => None
      }
    )
  and getInfixParselet = token =>
    Lexer.(
      switch (token.t) {
      | PLUS => Some(parseNaryInfix(Add))
      | MINUS => Some(parseNaryInfix(Add))
      | STAR => Some(parseNaryInfix(Mul))
      | SLASH => Some(parseBinaryInfix(Div))
      | CARET => Some(parseBinaryInfix(Exp))
      | _ => None
      }
    )
  and parseExpression = (precedence: int) => {
    let left =
      switch (getPrefixParselet(consume())) {
      | Some(parselet) => parselet()
      | None => raise(Error)
      };
    switch (getInfixParselet(peek())) {
    | Some(parselet) => parselet(precedence, left)
    | None => left
    };
  }
  and parsePrefix = (op, ()) =>
    Apply(Neg, [parseExpression(getOpPrecedence(op))])
  and parseNaryInfix = (op, precedence, left) =>
    switch (parseNaryArgs(op, precedence, peek())) {
    | [] => left
    | otherArgs => Apply(op, [left] @ otherArgs)
    }
  and parseNaryArgs = (op, precedence, token) : list(node) =>
    Lexer.(
      if (precedence < getPrecedence()) {
        consume() |> ignore;
        let result = parseExpression(getOpPrecedence(op));
        switch (token.t, peek().t) {
        | (PLUS, PLUS | MINUS) =>
          [result] @ parseNaryArgs(op, precedence, peek())
        | (MINUS, PLUS | MINUS) =>
          [Apply(Neg, [result])] @ parseNaryArgs(op, precedence, peek())
        | (a, b) when a == b =>
          [result] @ parseNaryArgs(op, precedence, peek())
        | (MINUS, _) => [Apply(Neg, [result])]
        | (_, _) => [result]
        };
      } else {
        [];
      }
    )
  and parseBinaryInfix = (op, precedence, left) : node =>
    if (precedence < getPrecedence()) {
      switch (getInfixParselet(consume())) {
      | Some(parselet) =>
        parselet(
          precedence,
          Apply(op, [left, parseExpression(getOpPrecedence(op))]),
        )
      | None => raise(Error)
      };
    } else {
      left;
    };
  parseExpression(0);
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