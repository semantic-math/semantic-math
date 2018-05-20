/**
 * Parser
 */
open Node;

module TokenTypeMap = Map.Make(TokenType);

type parser = {
  mutable tokens: array(Token.t),
  mutable index: int,
  peek: int => Token.t,
  consume: unit => Token.t,
  parseExpression: int => node,
}
and infix_parselet = {
  op: operator,
  parse: (parser, node) => node,
}
and prefix_parselet = {parse: (parser, Token.t) => node};

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

let parsePostfix = (op, parser, left) => {
  parser.consume() |> ignore;
  Apply(op, [left]);
};

let rec parseNaryInfix = (op, parser, left) =>
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

let parseBinaryInfix = (op, parser, left) => {
  parser.consume() |> ignore;
  Apply(op, [left, parser.parseExpression(getOpPrecedence(op))]);
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

let infixParseletMap =
  TokenTypeMap.empty
  |> TokenTypeMap.add(
       TokenType.MINUS,
       {op: Sub, parse: parseBinaryInfix(Sub)},
     )
  |> TokenTypeMap.add(
       TokenType.CARET,
       {op: Exp, parse: parseBinaryInfix(Exp)},
     )
  |> TokenTypeMap.add(
       TokenType.SLASH,
       {op: Div, parse: parseBinaryInfix(Div)},
     )
  |> TokenTypeMap.add(
       TokenType.UNDERSCORE,
       {op: Subscript, parse: parseBinaryInfix(Subscript)},
     )
  |> TokenTypeMap.add(
       TokenType.COMMA,
       {op: Comma, parse: parseNaryInfix(Comma)},
     )
  |> TokenTypeMap.add(TokenType.EQUAL, {op: Eq, parse: parseNaryInfix(Eq)})
  |> TokenTypeMap.add(
       TokenType.LESS_THAN,
       {op: Lt, parse: parseNaryInfix(Lt)},
     )
  |> TokenTypeMap.add(
       TokenType.GREATER_THAN,
       {op: Gt, parse: parseNaryInfix(Gt)},
     )
  |> TokenTypeMap.add(
       TokenType.LESS_THAN_OR_EQUAL,
       {op: Lte, parse: parseNaryInfix(Lte)},
     )
  |> TokenTypeMap.add(
       TokenType.GREATER_THAN_OR_EQUAL,
       {op: Gte, parse: parseNaryInfix(Gte)},
     )
  |> TokenTypeMap.add(
       TokenType.PLUS,
       {op: Add, parse: parseNaryInfix(Add)},
     )
  |> TokenTypeMap.add(
       TokenType.STAR,
       {op: Mul(`Explicit), parse: parseNaryInfix(Mul(`Explicit))},
     )
  |> TokenTypeMap.add(
       TokenType.IDENTIFIER,
       {op: Mul(`Implicit), parse: parseNaryInfix(Mul(`Implicit))},
     )
  |> TokenTypeMap.add(
       TokenType.ELLIPSES,
       {op: Mul(`Implicit), parse: parseNaryInfix(Mul(`Implicit))},
     )
  |> TokenTypeMap.add(
       TokenType.LEFT_PAREN,
       {
         op: Mul(`Implicit),
         parse: (parser, left) =>
           postProcessMulByParens(
             parser.peek(-1),
             [left] @ parseMulByParens(parser),
           ),
       },
     )
  |> TokenTypeMap.add(
       TokenType.RIGHT_PAREN,
       {op: Nul, parse: (_, _) => raise(UnmatchedRightParen)},
     )
  |> TokenTypeMap.add(
       TokenType.BANG,
       {op: Fact, parse: parsePostfix(Fact)},
     )
  |> TokenTypeMap.add(
       TokenType.SINGLE_QUOTE,
       {op: Prime, parse: parsePostfix(Prime)},
     );

let prefixParseletMap =
  TokenTypeMap.empty
  |> TokenTypeMap.add(
       TokenType.MINUS,
       {
         parse: (parser, _) =>
           Apply(Neg, [parser.parseExpression(getOpPrecedence(Neg))]),
       },
     )
  |> TokenTypeMap.add(
       TokenType.IDENTIFIER,
       {parse: (_, token) => Identifier(token.value)},
     )
  |> TokenTypeMap.add(
       TokenType.NUMBER,
       {parse: (_, token) => Number(token.value)},
     )
  |> TokenTypeMap.add(TokenType.ELLIPSES, {parse: (_, _) => Ellipses})
  |> TokenTypeMap.add(
       TokenType.LEFT_PAREN,
       {
         parse: (parser, _) => {
           let expr = parser.parseExpression(0);
           switch (parser.consume().t) {
           | RIGHT_PAREN => expr
           | _ => raise(UnmatchedLeftParen)
           };
         },
       },
     );

let getPrecedence = parser => {
  let token = parser.peek(0);
  if (TokenTypeMap.mem(token.t, infixParseletMap)) {
    let parselet = TokenTypeMap.find(token.t, infixParseletMap);
    getOpPrecedence(parselet.op);
  } else {
    0;
  };
};

let parseInfix = (parser, left) => {
  let token = parser.peek(0);
  if (TokenTypeMap.mem(token.t, infixParseletMap)) {
    let parselet = TokenTypeMap.find(token.t, infixParseletMap);
    parselet.parse(parser, left);
  } else {
    left;
  };
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

let make = () => {
  let eof = Token.make(EOF, "");
  let rec parseExpression = precedence => {
    let left = ref(parsePrefix(parser));
    while (precedence < getPrecedence(parser)) {
      left := parseInfix(parser, left^);
    };
    let result = left^;
    result;
  }
  and consume = () =>
    if (parser.index < Array.length(parser.tokens)) {
      let result = parser.tokens[parser.index];
      parser.index = parser.index + 1;
      result;
    } else {
      eof;
    }
  and peek = offset =>
    if (parser.index + offset < 0) {
      eof;
    } else if (parser.index + offset < Array.length(parser.tokens)) {
      parser.tokens[parser.index + offset];
    } else {
      eof;
    }
  and parser = {tokens: [||], index: 0, peek, consume, parseExpression};
  parser;
};

let parse = (parser, tokens: array(Token.t)) => {
  parser.tokens = Array.of_list(preprocessTokens(Array.to_list(tokens)));
  parser.index = 0;
  let result = parser.parseExpression(0);
  switch (parser.peek(0).t) {
  | EOF => ()
  | RIGHT_PAREN => raise(UnmatchedRightParen)
  | _ => raise(UnexpectedToken)
  };
  result;
};