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
  | Sub 
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
  | Neg => 7
  | Pos => 7
  /***
   * We give implicit multiplication higher precedence than division to support
   * parsing expressions like ab / cd as [/ [* a b] [* c d]] and higher than
   * Pos/Neg prefixes to support -(a)(b)(c) parsing as [neg [* a b c]].
   */
  | Mul(`Implicit) => 6
  | Exp => 9
  | Sub => 9
  | Func(_) => 10
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

let rec preprocessTokens = (tokens: list(Lexer.token)) =>
  Lexer.(
    switch (tokens) {
    | [hd, ...tl] =>
      switch (hd.t) {
      | IDENTIFIER(name) =>
        if (List.mem(name, Data.wellKnownIdentifiers)) {
          [hd, ...preprocessTokens(tl)];
        } else {
          switch (Array.to_list(Js.String.split("", name))) {
          | [_] => [hd, ...preprocessTokens(tl)]
          | letters =>
            List.map(
              letter => makeToken(IDENTIFIER(letter), letter),
              letters,
            )
            @ preprocessTokens(tl)
          };
        }
      | _ => [hd, ...preprocessTokens(tl)]
      }
    | [] => []
    }
  );

let parse = (tokens: array(Lexer.token)) => {
  /* TODO: instead of actually consuming tokens, just advance an index */
  let tokens = Array.of_list(preprocessTokens(Array.to_list(tokens)));
  let index = ref(0);
  let consume = () =>
    Lexer.(
      if (index^ < Array.length(tokens)) {
        let result = tokens[index^];
        index := index^ + 1;
        result;
      } else {
        makeToken(EOF, "");
      }
    );
  let peek = offset =>
    Lexer.(
      if (index^ + offset < 0) {
        /* TODO: make a dummy token */
        makeToken(EOF, "");
      } else if (index^ + offset < Array.length(tokens)) {
        tokens[index^ + offset];
      } else {
        makeToken(EOF, "");
      }
    );
  let getPrecedence = () =>
    Lexer.(
      switch (peek(0).t) {
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
      | UNDERSCORE => getOpPrecedence(Sub)
      | SLASH => getOpPrecedence(Div)
      | _ => 0
      }
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
      switch (peek(0).t) {
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
          switch (left, right) {
          | (Identifier(_), Apply(Comma, args)) => Apply(Func(left), args)
          | (Number(_), _) => Apply(Mul(`Implicit), children)
          | (Apply(Mul(`Implicit), factors), _) => 
            switch (List.rev(factors)) {
            /* Parse 2sin(x) to [* 2 [sin x]] */
            | [hd, ...tl] => Apply(Mul(`Implicit), List.rev([Apply(Func(hd), [right])] @ tl))
            | [] => raise(Unhandled) /* multiplication should always have 2 or more operands */
            }
          | _ => Apply(Func(left), [right])
          }
        | _ => Apply(Mul(`Implicit), children)
        };
      | IDENTIFIER(_) => parseNaryInfix(left, Mul(`Implicit))
      | CARET => parseBinaryInfix(left, Exp)
      | SLASH => parseBinaryInfix(left, Div)
      | UNDERSCORE => parseBinaryInfix(left, Sub)
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
    let token = peek(0);
    switch (token.t) {
    /* there is no token for the operator for implicit multiplication by identifier */
    | IDENTIFIER(_) => ()
    | _ => consume() |> ignore
    };
    let result = parseExpression(getOpPrecedence(op));
    switch (token.t, peek(0).t) {
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
      | IDENTIFIER(name) => Identifier(name)
        /***
         * Check if the token before the first identifier was a negative sign.
         * If it is, and the next token is an identifier, they we have something
         * like -abc.  We want to parse this is [neg [* a b c]].
         */
        /* switch (peek(-2).t, peek(0).t) {
        | (MINUS, IDENTIFIER(_)) =>
          Apply(
            Mul(`Implicit),
            [Identifier(name)] @ parseMulByIdentifier(),
          )
        | _ => Identifier(name)
        } */
      | NUMBER(value) => Number(value)
      | LEFT_PAREN =>
        let expr = parseExpression(0);
        switch (consume().t) {
        | RIGHT_PAREN =>
          switch (peek(0).t) {
          | LEFT_PAREN =>
            consume() |> ignore;
            Apply(Mul(`Implicit), [expr] @ parseMulByParens());
          | _ => expr
          }
        | _ => raise(Unhandled) /* unmatched left paren */
        };
      | _ => raise(Unhandled) /* unexpected token */
      }
    )
  and parseMulByParens = () => {
    let expr = parseExpression(0);
    switch (consume().t) {
    | RIGHT_PAREN =>
      switch (peek(0).t) {
      | LEFT_PAREN =>
        consume() |> ignore;
        [expr] @ parseMulByParens();
      | _ => [expr]
      }
    | _ => raise(Unhandled) /* unmatched left paren */
    };
  }
  and parseMulByIdentifier = () =>
    switch (peek(0).t) {
    | IDENTIFIER(name) =>
      consume() |> ignore;
      [Identifier(name)] @ parseMulByIdentifier();
    | _ => []
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
  | Sub => "_"
  | Func(name) => nodeToString(name)
  };