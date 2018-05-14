type node =
  | Apply(operator, list(node))
  | Identifier(string)
  | Number(string)
and operator =
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
  | Pos
  | Comma
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
  | Func(_) => 9
  };

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

let parse = tokens => {
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
      | COMMA => getOpPrecedence(Comma)
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
  let rec parseExpression = precedence => {
    let left = ref(parsePrefix());
    while (precedence < getPrecedence()) {
      left := parseInfix(left^);
    };
    let result = left^;
    result;
  }
  and parseInfix = left =>
    Lexer.(
      switch (peek().t) {
      | COMMA => parseNaryInfix(left, Comma)
      | EQUAL => parseNaryInfix(left, Eq)
      | LESS_THAN => parseNaryInfix(left, Lt)
      | GREATER_THAN => parseNaryInfix(left, Gt)
      | LESS_THAN_OR_EQUAL => parseNaryInfix(left, Lte)
      | GREATER_THAN_OR_EQUAL => parseNaryInfix(left, Gte)
      | PLUS => parseNaryInfix(left, Add)
      /***
       * Parse minus as addition, parseNaryArgs converts any minus signs
       * to neg(ation) operators.
       */
      | MINUS => parseNaryInfix(left, Add)
      | STAR => parseNaryInfix(left, Mul(`Explicit))
      | LEFT_PAREN =>
        consume() |> ignore;
        let children = [left] @ parseMulByParens();
        switch (children) {
        | [left, right] => 
          switch ((left, right)) {
          | (Identifier(_), Apply(Comma, args)) => Apply(Func(left), args)
          | (Number(_), _) => Apply(Mul(`Implicit), children)
          | _ => Apply(Func(left), [right])
          }
        | _ => Apply(Mul(`Implicit), children)
        };
      | IDENTIFIER(name) =>
        if (! List.mem(name, Data.wellKnownIdentifiers)) {
          consume() |> ignore; /* consume the un-split identifier */
          splitIdentifier(name);
        };
        parseNaryInfix(left, Mul(`Implicit));
      | CARET => parseBinaryInfix(left, Exp)
      | SLASH => parseBinaryInfix(left, Div)
      | RIGHT_PAREN => raise(Unhandled) /* unmatched right paren */
      | _ => left
      }
    )
  and parseNaryInfix = (left, op) => Apply(op, [left] @ parseNaryArgs(op))
  and parseBinaryInfix = (left, op) => {
    consume() |> ignore;
    Apply(op, [left, parseExpression(getOpPrecedence(op))]);
  }
  and parseNaryArgs = op => {
    open Lexer;
    let token = peek();
    switch (token.t) {
    /* there is no token for the operator for implicit multiplication by identifier */
    | IDENTIFIER(_) => ()
    | _ => consume() |> ignore
    };
    let result = parseExpression(getOpPrecedence(op));
    switch (token.t, peek().t) {
    | (PLUS, PLUS | MINUS) => [result] @ parseNaryArgs(op)
    | (MINUS, PLUS | MINUS) => [Apply(Neg, [result])] @ parseNaryArgs(op)
    | (NUMBER(_) | IDENTIFIER(_), IDENTIFIER(_)) =>
      [result] @ parseNaryArgs(op)
    | (a, b) when a == b => [result] @ parseNaryArgs(op)
    | (MINUS, _) => [Apply(Neg, [result])]
    | (_, _) => [result]
    };
  }
  and parsePrefix = () =>
    Lexer.(
      switch (consume().t) {
      | MINUS => Apply(Neg, [parseExpression(getOpPrecedence(Neg))])
      | IDENTIFIER(name) =>
        if (String.length(name) > 1
            && ! List.mem(name, Data.wellKnownIdentifiers)) {
          splitIdentifier(name);
          switch (consume().t) {
          | IDENTIFIER(letter) => parseInfix(Identifier(letter))
          | _ => raise(Unhandled)
          };
        } else {
          Identifier(name);
        }
      | NUMBER(value) => Number(value)
      | LEFT_PAREN =>
        let expr = parseExpression(0);
        switch (consume().t) {
        | RIGHT_PAREN => expr
        | _ => raise(Unhandled) /* unmatched left paren */
        };
      | _ => raise(Unhandled) /* unexpected token */
      }
    )
  and parseMulByParens = () => {
    let expr = parseExpression(0);
    switch (consume().t) {
    | RIGHT_PAREN =>
      switch (peek().t) {
      | LEFT_PAREN =>
        consume() |> ignore;
        [expr] @ parseMulByParens();
      | _ => [expr]
      }
    | _ => raise(Unhandled) /* unmatched left paren */
    };
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
  }
and opToString = op =>
  switch (op) {
  | Comma => ","
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
  | Func(name) => nodeToString(name)
  };