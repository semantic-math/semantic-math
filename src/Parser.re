type operator =
  | Eq
  | Lt
  | Gt
  | Lte
  | Gte
  | Add
  | Mul([ | `Implicit | `Explicit])
  | Div
  | Exp
  | Neg
  | Pos;

let getOpPrecedence = op =>
  switch (op) {
  | Eq => 1
  | Lt => 1
  | Gt => 1
  | Lte => 1
  | Gte => 1
  | Add => 3
  | Mul(`Explicit) => 4
  /***
   * We allow division to have higher precedence so that it's easy to write
   * multiplication of fractions e.g. x/y * a/b should parse as [* [/ x y] [/ a b]]
   */
  | Div => 5
  /***
   * We give implicit multiplication higher precedence than division to support
   * parsing expressions like ab / cd as [/ [* a b] [* c d]]
   */
  | Mul(`Implicit) => 6
  | Neg => 7
  | Pos => 7
  | Exp => 8
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
      | EQUAL => getOpPrecedence(Eq)
      | LESS_THAN => getOpPrecedence(Lt)
      | GREATER_THAN => getOpPrecedence(Gt)
      | LESS_THAN_OR_EQUAL => getOpPrecedence(Lte)
      | GREATER_THAN_OR_EQUAL => getOpPrecedence(Gte)
      | PLUS => getOpPrecedence(Add)
      | MINUS => getOpPrecedence(Add)
      | STAR => getOpPrecedence(Mul(`Explicit))
      | IDENTIFIER(_) => getOpPrecedence(Mul(`Implicit))
      | LEFT_PAREN => getOpPrecedence(Mul(`Implicit))
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
      | EQUAL => Apply(Eq, [left] @ parseNaryArgs(Eq, token))
      | LESS_THAN => Apply(Lt, [left] @ parseNaryArgs(Lt, token))
      | GREATER_THAN => Apply(Gt, [left] @ parseNaryArgs(Gt, token))
      | LESS_THAN_OR_EQUAL => Apply(Lte, [left] @ parseNaryArgs(Lte, token))
      | GREATER_THAN_OR_EQUAL => Apply(Gte, [left] @ parseNaryArgs(Gte, token))
      | PLUS => Apply(Add, [left] @ parseNaryArgs(Add, token))
      /***
       * Parse minus as addition, parseNaryArgs converts any minus signs
       * to neg(ation) operators.
       */
      | MINUS => Apply(Add, [left] @ parseNaryArgs(Add, token))
      | STAR => Apply(Mul(`Explicit), [left] @ parseNaryArgs(Mul(`Explicit), token))
      | LEFT_PAREN => Apply(Mul(`Implicit), [left] @ parseNaryArgs(Mul(`Implicit), peek()));
      | IDENTIFIER(name) =>
        consume() |> ignore; /* consume the un-split identifier */
        splitIdentifier(name);
        Apply(Mul(`Implicit), [left] @ parseNaryArgs(Mul(`Implicit), peek()));
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
        | _ => Apply(Mul(`Implicit), children)
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
  | Eq => "="
  | Lt => "<"
  | Gt => ">"
  | Lte => "<="
  | Gte => ">="
  | Add => "+"
  | Mul(_) => "*"
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