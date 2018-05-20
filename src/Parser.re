/**
 * Parser
 */
open Node;

exception Unhandled;

exception UnmatchedLeftParen;

exception UnmatchedRightParen;

exception UnexpectedToken;

let makeToken = (t, value) =>
  Token.{
    t,
    value,
    loc: {
      start: (-1),
      end_: (-1),
    },
  };

let rec preprocessTokens = (tokens: list(Token.t)) =>
  switch (tokens) {
  | [hd, ...tl] =>
    switch (hd.t) {
    | IDENTIFIER =>
      if (List.mem(hd.value, Data.wellKnownIdentifiers)) {
        [hd, ...preprocessTokens(tl)];
      } else {
        switch (Array.to_list(Js.String.split("", hd.value))) {
        | [_] => [hd, ...preprocessTokens(tl)]
        | letters =>
          List.map(letter => makeToken(IDENTIFIER, letter), letters)
          @ preprocessTokens(tl)
        };
      }
    | _ => [hd, ...preprocessTokens(tl)]
    }
  | [] => []
  };

module TokenTypeMap = Map.Make(TokenType);

type parselet_type =
  | Prefix
  | BinaryInfix
  | NaryInfix
  | Postfix;

type parselet = {
  t: parselet_type,
  op: operator,
};

let parseletMap =
  TokenTypeMap.empty
  |> TokenTypeMap.add(TokenType.MINUS, {t: BinaryInfix, op: Sub})
  |> TokenTypeMap.add(TokenType.CARET, {t: BinaryInfix, op: Exp})
  |> TokenTypeMap.add(TokenType.SLASH, {t: BinaryInfix, op: Div})
  |> TokenTypeMap.add(TokenType.UNDERSCORE, {t: BinaryInfix, op: Subscript})
  |> TokenTypeMap.add(TokenType.COMMA, {t: NaryInfix, op: Comma})
  |> TokenTypeMap.add(TokenType.EQUAL, {t: NaryInfix, op: Eq})
  |> TokenTypeMap.add(TokenType.LESS_THAN, {t: NaryInfix, op: Lt})
  |> TokenTypeMap.add(TokenType.GREATER_THAN, {t: NaryInfix, op: Gt})
  |> TokenTypeMap.add(TokenType.LESS_THAN_OR_EQUAL, {t: NaryInfix, op: Lte})
  |> TokenTypeMap.add(
       TokenType.GREATER_THAN_OR_EQUAL,
       {t: NaryInfix, op: Gte},
     )
  |> TokenTypeMap.add(TokenType.PLUS, {t: NaryInfix, op: Add})
  |> TokenTypeMap.add(TokenType.STAR, {t: NaryInfix, op: Mul(`Explicit)})
  |> TokenTypeMap.add(
       TokenType.IDENTIFIER,
       {t: NaryInfix, op: Mul(`Implicit)},
     )
  |> TokenTypeMap.add(
       TokenType.ELLIPSES,
       {t: NaryInfix, op: Mul(`Implicit)},
     )
  |> TokenTypeMap.add(TokenType.BANG, {t: Postfix, op: Fact})
  |> TokenTypeMap.add(TokenType.SINGLE_QUOTE, {t: Postfix, op: Prime});

let parse = (tokens: array(Token.t)) => {
  /* TODO: instead of actually consuming tokens, just advance an index */
  let tokens = Array.of_list(preprocessTokens(Array.to_list(tokens)));
  let index = ref(0);
  let consume = () =>
    if (index^ < Array.length(tokens)) {
      let result = tokens[index^];
      index := index^ + 1;
      result;
    } else {
      makeToken(EOF, "");
    };
  let peek = offset =>
    if (index^ + offset < 0) {
      /* TODO: make a dummy token */
      makeToken(EOF, "");
    } else if (index^ + offset < Array.length(tokens)) {
      tokens[index^ + offset];
    } else {
      makeToken(EOF, "");
    };
  let getPrecedence = () =>
    switch (peek(0).t) {
    | LEFT_PAREN => getOpPrecedence(Mul(`Implicit))
    | token_type =>
      if (TokenTypeMap.mem(token_type, parseletMap)) {
        let parselet = TokenTypeMap.find(token_type, parseletMap);
        getOpPrecedence(parselet.op);
      } else {
        0;
      }
    };
  let rec parseExpression = precedence => {
    let left = ref(parsePrefix());
    while (precedence < getPrecedence()) {
      left := parseInfix(left^);
    };
    let result = left^;
    result;
  }
  and parsePrefix = () => {
    let token = consume();
    switch (token.t) {
    | MINUS => Apply(Neg, [parseExpression(getOpPrecedence(Neg))])
    | IDENTIFIER => Identifier(token.value)
    | NUMBER => Number(token.value)
    | ELLIPSES => Ellipses
    | LEFT_PAREN =>
      let expr = parseExpression(0);
      switch (consume().t) {
      | RIGHT_PAREN => expr
      | _ => raise(UnmatchedLeftParen)
      };
    | _ => raise(UnexpectedToken)
    };
  }
  and parseInfix = left =>
    switch (peek(0).t) {
    | LEFT_PAREN =>
      postProcessMulByParens(peek(-1), [left] @ parseMulByParens())
    | RIGHT_PAREN => raise(UnmatchedRightParen)
    | token_type =>
      if (TokenTypeMap.mem(token_type, parseletMap)) {
        let parselet = TokenTypeMap.find(token_type, parseletMap);
        switch (parselet.t) {
        | BinaryInfix => parseBinaryInfix(left, parselet.op)
        | NaryInfix => parseNaryInfix(left, parselet.op)
        | Postfix => parsePostfix(left, parselet.op)
        | _ => raise(Unhandled)
        };
      } else {
        left;
      }
    }
  and parseBinaryInfix = (left, op) => {
    consume() |> ignore;
    Apply(op, [left, parseExpression(getOpPrecedence(op))]);
  }
  and parsePostfix = (left, op) => {
    consume() |> ignore;
    Apply(op, [left]);
  }
  and parseNaryInfix = (left, op) => Apply(op, [left] @ parseNaryArgs(op))
  and parseNaryArgs = op => {
    let token = peek(0);
    switch (token.t) {
    /* there is no token for the operator for implicit multiplication */
    | IDENTIFIER => ()
    | ELLIPSES => ()
    | _ => consume() |> ignore
    };
    let expr = parseExpression(getOpPrecedence(op));
    switch (op, peek(0).t) {
    | (Add, PLUS) => [expr] @ parseNaryArgs(op)
    | (Mul(`Implicit), IDENTIFIER | ELLIPSES) => [expr] @ parseNaryArgs(op)
    | (_, t) when token.t == t => [expr] @ parseNaryArgs(op)
    | _ => [expr]
    };
  }
  and parseMulByParens = () => {
    let expr = parseExpression(getOpPrecedence(Mul(`Implicit)));
    switch (peek(0).t) {
    | LEFT_PAREN
    | ELLIPSES => [expr] @ parseMulByParens()
    | _ => [expr]
    };
  }
  and postProcessMulByParens = (prevToken, children) =>
    switch (children) {
    | [left, right] =>
      switch (left, right) {
      | (Identifier(_), Apply(Comma, args)) => Apply(Func(left), args)
      | (Number(_), _) => Apply(Mul(`Implicit), children)
      | (Apply(Fact, _), _) => Apply(Mul(`Implicit), children)
      | (Apply(Mul(`Implicit), factors), _) =>
        /* Parse 2x sin(x) to [* 2 x [sin x]] */
        switch (List.rev(factors)) {
        | [hd, ...tl] =>
          Apply(
            Mul(`Implicit),
            List.rev([Apply(Func(hd), [right])] @ tl),
          )
        | [] => raise(Unhandled) /* multiplication should always have 2 or more operands */
        }
      | _ =>
        /* parse (a)(b) as multiplication for now */
        /* TODO: allow (f + g)(x) to be parsed as a function */
        switch (prevToken.t) {
        | RIGHT_PAREN => Apply(Mul(`Implicit), children)
        | _ => Apply(Func(left), [right])
        }
      }
    | _ => Apply(Mul(`Implicit), children)
    };
  /* start parsing */
  let result = parseExpression(0);
  switch (peek(0).t) {
  | RIGHT_PAREN => raise(UnmatchedRightParen)
  | t when t != EOF => raise(UnexpectedToken) /* unexpected token */
  | _ => ()
  };
  result;
};