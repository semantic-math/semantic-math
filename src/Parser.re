/**
 * Parser - A generalized Pratt Parser
 */
open Node;

module TokenTypeMap = Map.Make(TokenType);

type parser = {
  mutable tokens: array(Token.t),
  mutable index: int,
  peek: int => Token.t,
  consume: unit => Token.t,
  parse: int => node,
  infixParseletMap: TokenTypeMap.t(infix_parselet),
  mutable prefixParseletMap: TokenTypeMap.t(prefix_parselet),
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

let getPrecedence = parser => {
  let token = parser.peek(0);
  if (TokenTypeMap.mem(token.t, parser.infixParseletMap)) {
    let parselet = TokenTypeMap.find(token.t, parser.infixParseletMap);
    getOpPrecedence(parselet.op);
  } else {
    0;
  };
};

let parseInfix = (parser, left) => {
  let token = parser.peek(0);
  if (TokenTypeMap.mem(token.t, parser.infixParseletMap)) {
    let parselet = TokenTypeMap.find(token.t, parser.infixParseletMap);
    parselet.parse(parser, left);
  } else {
    left;
  };
};

let parsePrefix = parser => {
  let token = parser.consume();
  if (TokenTypeMap.mem(token.t, parser.prefixParseletMap)) {
    let parselet = TokenTypeMap.find(token.t, parser.prefixParseletMap);
    parselet.parse(parser, token);
  } else {
    raise(UnexpectedToken);
  };
};

let make = (prefixParseletMap, infixParseletMap) => {
  let eof = Token.make(EOF, "");
  let rec parse = precedence => {
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
  and parser = {
    tokens: [||],
    index: 0,
    peek,
    consume,
    parse,
    prefixParseletMap: prefixParseletMap,
    infixParseletMap: infixParseletMap,
  };
  parser;
};

let parse = (parser, tokens: array(Token.t)) => {
  parser.tokens = tokens;
  parser.index = 0;
  let result = parser.parse(0);
  switch (parser.peek(0).t) {
  | EOF => ()
  | RIGHT_PAREN => raise(UnmatchedRightParen)
  | _ => raise(UnexpectedToken)
  };
  result;
};