/* TODO(kevinb): store the type signatures of operators for semantic analysis */
type node =
  | Apply(operator, array(node))
  | Identifier(string)
  | Number(string)
and operator =
  | Add
  | Sub
  | Mul([ | `Explicit | `Implicit])
  | Div
  | Exp
  | Neg
  | Pos
  | Eq
  | Gt
  | Lt
  | Func(node)
  | /**
   * These aren't actually operators but they do appear temporarily in the
   * operandStack so we include them here to handle those situations.
   */
    LeftParen
  | RightParen
  | Comma;

let rec nodeToString = node =>
  switch (node) {
  | Apply(op, children) =>
    "["
    ++ Js.Array.joinWith(
         " ",
         Array.append(
           [|opToString(op)|],
           Array.map(nodeToString, children),
         ),
       )
    ++ "]"
  | Identifier(name) => name
  | Number(value) => value
  }
and opToString = (op: operator) =>
  switch (op) {
  | Add => "+"
  | Sub => "-"
  | Mul(_) => "*"
  | Div => "/"
  | Neg => "neg"
  | Pos => "pos"
  | Exp => "^"
  | Eq => "="
  | LeftParen => "("
  | RightParen => ")"
  | Gt => ">"
  | Lt => "<"
  | Func(node) => nodeToString(node)
  | Comma => ","
  };

let precedence = op =>
  switch (op) {
  | Comma => (-1)
  | Eq
  | Lt
  | Gt => 0
  | LeftParen
  | RightParen => 1
  | Add
  | Sub => 2
  | Mul(`Explicit) => 3
  | Mul(`Implicit)
  | Div => 4
  | Neg
  | Pos => 5
  | Exp => 6
  | Func(_) => 7
  };

let wellKnownIdentifiers = [
  /* greek letters */
  "alpha",
  "beta",
  "gamma",
  "delta",
  "epsilon",
  "pi",
  "tau",
  "theta",
  /* trig functions */
  /* TODO(kevinb): store the type signature of functions for semantic analysis */
  "sin",
  "cos",
  "tan",
  "sec",
  "csc",
  "cot",
  "asin",
  "acos",
  "atan",
  "asec",
  "acsc",
  "acot",
];

exception Missing_operator;

exception Missing_operand;

exception Unmatched_left_paren;

exception Unmatched_right_paren;

exception Unknown_error;

exception Unknown_operator(string);

exception Empty_identifier;

let replaceTop = (stack, value) => {
  Belt.MutableStack.pop(stack) |> ignore;
  Belt.MutableStack.push(stack, value);
};

let last = arr =>
  switch (Array.length(arr)) {
  | 0 => None
  | n => Some(arr[n - 1])
  };

module Stack = Belt.MutableStack;

/* return a node */
let parse = (tokens: array(Lexer.token)) => {
  let operatorStack = Stack.make();
  let operandStack: Stack.t(node) = Stack.make();
  let popOperands = arity : array(node) => {
    let children = [||];
    for (_ in 1 to arity) {
      switch (Belt.MutableStack.pop(operandStack)) {
      | Some(value) => Js.Array.push(value, children) |> ignore
      | None => raise(Missing_operand)
      };
    };
    /* reverse the children so they're in the right order */
    Js.Array.reverseInPlace(children) |> ignore;
    children;
  };
  /**
   * Pop all operations or until the first left parenthesis is encountered.String
   *
   * As operations are popped from the operationStack, Apply(op, array(node))
   * nodes are created based on the arity of op and pushed on to operandStack.
   */
  let popOperations = (~all: bool) => {
    let break = ref(false);
    while (! break^) {
      switch (Stack.pop(operatorStack)) {
      | Some((op, arity)) =>
        switch (op) {
        | LeftParen => all ? raise(Unmatched_left_paren) : break := true
        | _ =>
          let children = popOperands(arity);
          switch (op, children) {
          | (Func(_), [|Apply(Comma, args)|]) =>
            /**
             * Transform Func nodes as follows: [f [, x y]] => [f x y]
             */
            Stack.push(operandStack, Apply(op, args)) /* multiple args */
          | _ => Stack.push(operandStack, Apply(op, children))
          };
        }
      | None => all ? break := true : raise(Unmatched_right_paren)
      };
    };
  };
  /* replace `a - b` with `a + neg b` */
  let tokens: array(Lexer.token) =
    Array.fold_left(
      (accum, token) => {
        switch (token.Lexer.t) {
        | Lexer.MINUS =>
          switch (last(accum)) {
          | Some(prevToken)
              when prevToken.Lexer.t != Lexer.MINUS && prevToken.Lexer.t != Lexer.LEFT_PAREN =>
            Js.Array.push({
              Lexer.t: Lexer.PLUS,
              Lexer.value: "+",
              Lexer.loc: {
                Lexer.start: -1,
                Lexer.end_: -1,
              },
            }, accum) |> ignore
          | _ => ()
          };
          Js.Array.push({
            Lexer.t: Lexer.MINUS,
            Lexer.value: "-",
            Lexer.loc: {
              Lexer.start: -1,
              Lexer.end_: -1,
            },
          }, accum) |> ignore;
        | _ => Js.Array.push(token, accum) |> ignore
        };
        accum;
      },
      [||],
      tokens,
    );
  /* Array.iter(token => Js.log(token), tokens); */
  /**
   * op: Lexer.token - operator to parse
   * ~collate: boolean - combine multiple operators into n-ary operator
   */
  let parseOp = (~collate=false, op) =>
    switch (Stack.top(operatorStack)) {
    | Some((topOp, arity)) when op == topOp && collate =>
      replaceTop(operatorStack, (op, arity + 1))
    | Some((topOp, arity))
        when topOp != LeftParen && precedence(op) < precedence(topOp) =>
      Stack.pop(operatorStack) |> ignore;
      Stack.push(operandStack, Apply(topOp, popOperands(arity)));
      /* case where the revealed operator matches the new operator */
      switch (Stack.top(operatorStack)) {
      | Some((topOp, arity)) when topOp == op =>
        replaceTop(operatorStack, (op, arity + 1))
      | _ => Stack.push(operatorStack, (op, 2))
      };
    | _ => Stack.push(operatorStack, (op, 2))
    };
  /* Process each token. */
  tokens
  |> Array.iteri((i, token) => {
       let prevToken = i > 0 ? Some(tokens[i - 1]) : None;
       let nextToken =
         i < Array.length(tokens) - 1 ? Some(tokens[i + 1]) : None;
       switch (token.Lexer.t) {
       | Lexer.IDENTIFIER(name) =>
         switch (String.length(name)) {
         | 0 => raise(Empty_identifier)
         | 1 =>
           switch (nextToken) {
           | Some({Lexer.t: Lexer.LEFT_PAREN}) =>
             /* function */
             Stack.push(operatorStack, (Func(Identifier(name)), 1))
           | _ => Stack.push(operandStack, Identifier(name))
           }
         | _ when ! List.mem(name, wellKnownIdentifiers) =>
           /* turn multi-character identifiers into multiplication */
           let letters = Js.String.split("", name);
           Stack.push(operandStack, Identifier(letters[0]));
           for (j in 1 to Array.length(letters) - 1) {
             parseOp(~collate=true, Mul(`Implicit));
             Stack.push(operandStack, Identifier(letters[j]));
           };
         | _ =>
           switch (nextToken) {
           | Some({Lexer.t: Lexer.LEFT_PAREN}) =>
             /* function */
             Stack.push(operatorStack, (Func(Identifier(name)), 1))
           | _ => Stack.push(operandStack, Identifier(name))
           }
         }
       | Lexer.NUMBER(value) =>
         Stack.push(operandStack, Number(value));
         switch (nextToken) {
         | Some({Lexer.t: Lexer.IDENTIFIER(_)}) =>
           parseOp(~collate=true, Mul(`Implicit))
         | _ => ()
         };
       | Lexer.RIGHT_PAREN => popOperations(~all=false)
       | Lexer.LEFT_PAREN =>
         switch (prevToken) {
         | Some({Lexer.t: Lexer.RIGHT_PAREN}) => parseOp(~collate=true, Mul(`Implicit))
         | _ => ()
         };
         /* TODO(kevinb): post-process implicit multiplication to detect Funcs */
         Stack.push(operatorStack, (LeftParen, 0));
       | Lexer.PLUS => parseOp(~collate=true, Add)
       | Lexer.STAR => parseOp(~collate=true, Mul(`Explicit))
       | Lexer.MINUS => Stack.push(operatorStack, (Neg, 1))
       | Lexer.CARET => parseOp(Exp)
       | Lexer.EQUAL => parseOp(~collate=true, Eq)
       | Lexer.COMMA => parseOp(~collate=true, Comma)
       | Lexer.SLASH => parseOp(Div)
       | op => raise(Unknown_operator(Lexer.tokenTypeToString(op)))
       };
     });
  /* Clean up any operators that are still on the operator stack. */
  popOperations(~all=true);
  /* Check if we have a single value left and return that value in that case. */
  switch (Stack.size(operandStack)) {
  | 0 => raise(Missing_operand)
  | 1 =>
    switch (Stack.pop(operandStack)) {
    | Some(value) => value
    | None => raise(Unknown_error) /* should never happen b/c we checked the size already */
    }
  | _ => raise(Missing_operator)
  };
};
