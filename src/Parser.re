/**
 * Parser
 */
open Node;

type parser = {
  peek: int => Token.t,
  consume: unit => Token.t,
  parseExpression: int => node,
};

type parselet_type =
  | Prefix
  | BinaryInfix
  | NaryInfix
  | Postfix;

type parselet = {
  t: parselet_type,
  op: operator,
};

type prefix_parselet = {
  t: parselet_type,
  parse: (parser, Token.t) => node,
};

exception Unhandled;

exception UnmatchedLeftParen;

exception UnmatchedRightParen;

exception UnexpectedToken;

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
          List.map(letter => Token.make(IDENTIFIER, letter), letters)
          @ preprocessTokens(tl)
        };
      }
    | _ => [hd, ...preprocessTokens(tl)]
    }
  | [] => []
  };

module TokenTypeMap = Map.Make(TokenType);

let infixParseletMap =
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

let prefixParseletMap =
  TokenTypeMap.empty
  |> TokenTypeMap.add(
       TokenType.MINUS,
       {
         t: Prefix,
         parse: (parser, _) =>
           Apply(Neg, [parser.parseExpression(getOpPrecedence(Neg))]),
       },
     )
  |> TokenTypeMap.add(
       TokenType.IDENTIFIER,
       {t: Prefix, parse: (_, token) => Identifier(token.value)},
     )
  |> TokenTypeMap.add(
       TokenType.NUMBER,
       {t: Prefix, parse: (_, token) => Number(token.value)},
     )
  |> TokenTypeMap.add(
       TokenType.ELLIPSES,
       {t: Prefix, parse: (_, _) => Ellipses},
     )
  |> TokenTypeMap.add(
    TokenType.LEFT_PAREN,
    {
      t: Prefix, 
      parse: (parser, _) => {
        let expr = parser.parseExpression(0);
        switch (parser.consume().t) {
        | RIGHT_PAREN => expr
        | _ => raise(UnmatchedLeftParen)
        };
      },
    });

let parseBinaryInfix = (parser, left, op) => {
  parser.consume() |> ignore;
  Apply(op, [left, parser.parseExpression(getOpPrecedence(op))]);
};

let getPrecedence = parser =>
  switch (parser.peek(0).t) {
  | LEFT_PAREN => getOpPrecedence(Mul(`Implicit))
  | token_type =>
    if (TokenTypeMap.mem(token_type, infixParseletMap)) {
      let parselet = TokenTypeMap.find(token_type, infixParseletMap);
      getOpPrecedence(parselet.op);
    } else {
      0;
    }
  };

let parsePostfix = (parser, left, op) => {
  parser.consume() |> ignore;
  Apply(op, [left]);
};

let rec parseNaryInfix = (parser, left, op) =>
  Apply(op, [left] @ parseNaryArgs(parser, op))
and parseNaryArgs = (parser, op) => {
  let token = parser.peek(0);
  switch (token.t) {
  /* there is no token for the operator for implicit multiplication */
  | IDENTIFIER => ()
  | ELLIPSES => ()
  | _ => parser.consume() |> ignore
  };
  let expr = parser.parseExpression(getOpPrecedence(op));
  switch (op, parser.peek(0).t) {
  | (Add, PLUS) => [expr] @ parseNaryArgs(parser, op)
  | (Mul(`Implicit), IDENTIFIER | ELLIPSES) =>
    [expr] @ parseNaryArgs(parser, op)
  | (_, t) when token.t == t => [expr] @ parseNaryArgs(parser, op)
  | _ => [expr]
  };
};

let rec parseMulByParens = parser => {
  let expr = parser.parseExpression(getOpPrecedence(Mul(`Implicit)));
  switch (parser.peek(0).t) {
  | LEFT_PAREN
  | ELLIPSES => [expr] @ parseMulByParens(parser)
  | _ => [expr]
  };
};

let postProcessMulByParens = (prevToken: Token.t, children) =>
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
        Apply(Mul(`Implicit), List.rev([Apply(Func(hd), [right])] @ tl))
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

let parseInfix = (parser, left) =>
  switch (parser.peek(0).t) {
  | LEFT_PAREN =>
    postProcessMulByParens(
      parser.peek(-1),
      [left] @ parseMulByParens(parser),
    )
  | RIGHT_PAREN => raise(UnmatchedRightParen)
  | token_type =>
    if (TokenTypeMap.mem(token_type, infixParseletMap)) {
      let parselet = TokenTypeMap.find(token_type, infixParseletMap);
      switch (parselet.t) {
      | BinaryInfix => parseBinaryInfix(parser, left, parselet.op)
      | NaryInfix => parseNaryInfix(parser, left, parselet.op)
      | Postfix => parsePostfix(parser, left, parselet.op)
      | _ => raise(Unhandled)
      };
    } else {
      left;
    }
  };

let parsePrefix = parser => {
  let token = parser.consume();
  if (TokenTypeMap.mem(token.t, prefixParseletMap)) {
    let parselet = TokenTypeMap.find(token.t, prefixParseletMap);
    parselet.parse(parser, token);
  } else {
    raise(UnexpectedToken);
  };
};

let parse = (tokens: array(Token.t)) => {
  /* TODO: instead of actually consuming tokens, just advance an index */
  let tokens = Array.of_list(preprocessTokens(Array.to_list(tokens)));
  let index = ref(0);
  let eof = Token.make(EOF, "");
  let consume = () =>
    if (index^ < Array.length(tokens)) {
      let result = tokens[index^];
      index := index^ + 1;
      result;
    } else {
      eof;
    };
  let peek = offset =>
    if (index^ + offset < 0) {
      /* TODO: make a dummy token */
      eof;
    } else if (index^ + offset < Array.length(tokens)) {
      tokens[index^ + offset];
    } else {
      eof;
    };
  let rec parseExpression = precedence => {
    let left = ref(parsePrefix(parser));
    while (precedence < getPrecedence(parser)) {
      left := parseInfix(parser, left^);
    };
    let result = left^;
    result;
  }
  and parser = {peek, consume, parseExpression};
  /* start parsing */
  let result = parseExpression(0);
  switch (parser.peek(0).t) {
  | RIGHT_PAREN => raise(UnmatchedRightParen)
  | t when t != EOF => raise(UnexpectedToken) /* unexpected token */
  | _ => ()
  };
  result;
};