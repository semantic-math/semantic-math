/**
 * MathParser - A specialized version of Parser for parsing math
 */
open Node;

open Parser;

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
  let expr = parser.parse(getOpPrecedence(op));
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
  Apply(op, [left, parser.parse(getOpPrecedence(op))]);
};

let rec parseMulByParens = (parser: Parser.parser) => {
  let expr = parser.parse(getOpPrecedence(Mul(`Implicit)));
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

let prefixParseletMap =
  TokenType.(
    TokenTypeMap.empty
    |> TokenTypeMap.add(
         MINUS,
         {
           parse: (parser, _) =>
             Apply(Neg, [parser.parse(getOpPrecedence(Neg))]),
         },
       )
    |> TokenTypeMap.add(
         IDENTIFIER,
         {parse: (_, token) => Identifier(token.value)},
       )
    |> TokenTypeMap.add(
         NUMBER,
         {parse: (_, token) => Number(token.value)},
       )
    |> TokenTypeMap.add(ELLIPSES, {parse: (_, _) => Ellipses})
    |> TokenTypeMap.add(
         LEFT_PAREN,
         {
           parse: (parser, _) => {
             let expr = parser.parse(0);
             switch (parser.consume().t) {
             | RIGHT_PAREN => expr
             | _ => raise(UnmatchedLeftParen)
             };
           },
         },
       )
  );

let infixParseletMap =
  TokenType.(
    TokenTypeMap.empty
    |> TokenTypeMap.add(MINUS, {op: Sub, parse: parseBinaryInfix(Sub)})
    |> TokenTypeMap.add(CARET, {op: Exp, parse: parseBinaryInfix(Exp)})
    |> TokenTypeMap.add(SLASH, {op: Div, parse: parseBinaryInfix(Div)})
    |> TokenTypeMap.add(
         UNDERSCORE,
         {op: Subscript, parse: parseBinaryInfix(Subscript)},
       )
    |> TokenTypeMap.add(COMMA, {op: Comma, parse: parseNaryInfix(Comma)})
    |> TokenTypeMap.add(EQUAL, {op: Eq, parse: parseNaryInfix(Eq)})
    |> TokenTypeMap.add(LESS_THAN, {op: Lt, parse: parseNaryInfix(Lt)})
    |> TokenTypeMap.add(GREATER_THAN, {op: Gt, parse: parseNaryInfix(Gt)})
    |> TokenTypeMap.add(
         LESS_THAN_OR_EQUAL,
         {op: Lte, parse: parseNaryInfix(Lte)},
       )
    |> TokenTypeMap.add(
         GREATER_THAN_OR_EQUAL,
         {op: Gte, parse: parseNaryInfix(Gte)},
       )
    |> TokenTypeMap.add(PLUS, {op: Add, parse: parseNaryInfix(Add)})
    |> TokenTypeMap.add(
         STAR,
         {op: Mul(`Explicit), parse: parseNaryInfix(Mul(`Explicit))},
       )
    |> TokenTypeMap.add(
         IDENTIFIER,
         {op: Mul(`Implicit), parse: parseNaryInfix(Mul(`Implicit))},
       )
    |> TokenTypeMap.add(
         ELLIPSES,
         {op: Mul(`Implicit), parse: parseNaryInfix(Mul(`Implicit))},
       )
    |> TokenTypeMap.add(
         LEFT_PAREN,
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
         RIGHT_PAREN,
         {op: Nul, parse: (_, _) => raise(UnmatchedRightParen)},
       )
    |> TokenTypeMap.add(BANG, {op: Fact, parse: parsePostfix(Fact)})
    |> TokenTypeMap.add(
         SINGLE_QUOTE,
         {op: Prime, parse: parsePostfix(Prime)},
       )
  );

let parser = Parser.make(prefixParseletMap, infixParseletMap);

/**
 * Split any identifiers that aren't well-known identifiers into individual
 * letters.
 */
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

let parse = (tokens: array(Token.t)) =>
  Parser.parse(
    parser,
    Array.of_list(preprocessTokens(Array.to_list(tokens))),
  );