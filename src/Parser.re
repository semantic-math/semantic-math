/**
 * Parser
 */
open Node;

exception Unhandled;

exception UnmatchedLeftParen;

exception UnmatchedRightParen;

exception UnexpectedToken;

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
      | MINUS => getOpPrecedence(Sub)
      | STAR => getOpPrecedence(Mul(`Explicit))
      | IDENTIFIER(_) => getOpPrecedence(Mul(`Implicit))
      | ELLIPSES => getOpPrecedence(Mul(`Implicit))
      | LEFT_PAREN => getOpPrecedence(Mul(`Implicit))
      | CARET => getOpPrecedence(Exp)
      | UNDERSCORE => getOpPrecedence(Subscript)
      | SLASH => getOpPrecedence(Div)
      | BANG => getOpPrecedence(Fact)
      | SINGLE_QUOTE => getOpPrecedence(Prime)
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
  and parsePrefix = () =>
    Lexer.(
      switch (consume().t) {
      | MINUS => Apply(Neg, [parseExpression(getOpPrecedence(Neg))])
      | IDENTIFIER(name) => Identifier(name)
      | NUMBER(value) => Number(value)
      | ELLIPSES => Ellipses
      | LEFT_PAREN =>
        let expr = parseExpression(0);
        switch (consume().t) {
        | RIGHT_PAREN => expr
        | _ => raise(UnmatchedLeftParen)
        };
      | _ => raise(UnexpectedToken)
      }
    )
  and parseInfix = left =>
    Lexer.(
      switch (peek(0).t) {
      /* n-ary infix */
      | COMMA => parseNaryInfix(left, Comma)
      | EQUAL => parseNaryInfix(left, Eq)
      | LESS_THAN => parseNaryInfix(left, Lt)
      | GREATER_THAN => parseNaryInfix(left, Gt)
      | LESS_THAN_OR_EQUAL => parseNaryInfix(left, Lte)
      | GREATER_THAN_OR_EQUAL => parseNaryInfix(left, Gte)
      | PLUS => parseNaryInfix(left, Add)
      | STAR => parseNaryInfix(left, Mul(`Explicit))
      | IDENTIFIER(_) => parseNaryInfix(left, Mul(`Implicit))
      | ELLIPSES => parseNaryInfix(left, Mul(`Implicit))
      | LEFT_PAREN =>
        postProcessMulByParens(peek(-1), [left] @ parseMulByParens())
      /* binary infix */
      | MINUS => parseBinaryInfix(left, Sub)
      | CARET => parseBinaryInfix(left, Exp)
      | SLASH => parseBinaryInfix(left, Div)
      | UNDERSCORE => parseBinaryInfix(left, Subscript)
      /* postfix */
      | BANG => parsePostfix(left, Fact)
      | SINGLE_QUOTE => parsePostfix(left, Prime)
      | RIGHT_PAREN => raise(UnmatchedRightParen)
      | _ => left
      }
    )
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
    open Lexer;
    let token = peek(0);
    switch (token.t) {
    /* there is no token for the operator for implicit multiplication */
    | IDENTIFIER(_) => ()
    | ELLIPSES => ()
    | _ => consume() |> ignore
    };
    let expr = parseExpression(getOpPrecedence(op));
    switch (op, peek(0).t) {
    | (Add, PLUS) => [expr] @ parseNaryArgs(op)
    | (Mul(`Implicit), IDENTIFIER(_) | ELLIPSES) =>
      [expr] @ parseNaryArgs(op)
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
  | Ellipses => "..."
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
  | Sub => "-"
  | Mul(_) => "*"
  | Neg => "neg"
  | Pos => "pos"
  | Div => "/"
  | Exp => "^"
  | Subscript => "_"
  | Fact => "!"
  | Prime => "'"
  | Func(name) => nodeToString(name)
  };

let rec nodeToJson = node =>
  Json.Encode.(
    switch (node) {
    | Apply(op, args) =>
      object_([
        ("type", string("Apply")),
        ("op", opToJson(op)),
        ("args", jsonArray(Array.map(nodeToJson, Array.of_list(args)))),
      ])
    | Number(value) =>
      object_([("type", string("Number")), ("value", string(value))])
    | Identifier(name) =>
      object_([("type", string("Identifier")), ("name", string(name))])
    | Ellipses => object_([("type", string("Ellipses"))])
    }
  )
and opToJson = op : Js.Json.t =>
  Json.Encode.(
    switch (op) {
    | Comma => string("comma")
    | Eq => string("eq")
    | Lt => string("lt")
    | Gt => string("gt")
    | Lte => string("lte")
    | Gte => string("gte")
    | Add => string("add")
    | Sub => string("sub")
    | Mul(_) => string("mul")
    | Neg => string("neg")
    | Pos => string("pos")
    | Div => string("div")
    | Exp => string("exp")
    | Subscript => string("subscript")
    | Fact => string("fact")
    | Prime => string("prime")
    | Func(name) => nodeToJson(name)
    }
  );
