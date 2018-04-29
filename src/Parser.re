type operator =
  | Add
  | Sub
  /**
   * Mul(true) is implicit multiplication
   * Mul(false) is explicit multiplication
   */
  | Mul(bool)
  | Div
  | Exp
  | Neg
  | Pos
  | Eq
  | Gt
  | Lt
  /**
   * These aren't actually operators but they do appear temporarily in the
   * operandStack so we include them here to handle those situations.
   */
  | LeftParen
  | RightParen;

type node =
  | Apply(operator, array(node))
  | Identifier(string)
  | Number(string);

let precedence = op =>
  switch (op) {
  | Eq
  | Lt
  | Gt => 0
  | LeftParen
  | RightParen => 1
  | Add
  | Sub => 2
  | Mul(false) => 3
  | Mul(true)
  | Div => 4
  | Neg
  | Pos => 5
  | Exp => 6
  };

exception Missing_operator;

exception Missing_operand;

exception Unmatched_left_paren;

exception Unmatched_right_paren;

exception Unknown_error;

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
let parse = tokens => {
  let operatorStack = Stack.make();
  let operandStack = Stack.make();
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
  /* replace `a - b` with `a + neg b` */
  let tokens =
    Array.fold_left(
      (accum, token) => {
        switch (token) {
        | Lexer.MINUS =>
          switch (last(accum)) {
          | Some(token) when token != Lexer.MINUS && token != Lexer.LEFT_PAREN =>
            Js.Array.push(Lexer.PLUS, accum) |> ignore
          | _ => ()
          };
          Js.Array.push(Lexer.MINUS, accum) |> ignore;
        | _ => Js.Array.push(token, accum) |> ignore
        };
        accum;
      },
      [||],
      tokens,
    );
  /* Array.iter(token => Js.log(token), tokens); */
  /**
   * op: Lexer.token - binary operator to parse
   * ~collate: boolean - combine multiple operators into n-ary operator
   */
  let parseBinaryOp = (~collate=false, op) =>
    switch (Stack.top(operatorStack)) {
    | Some((topOp, arity)) when op == topOp && collate =>
      replaceTop(operatorStack, (op, arity + 1))
    | Some((topOp, arity)) when precedence(op) < precedence(topOp) =>
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
       switch (token) {
       | Lexer.IDENTIFIER(name) =>
         switch (String.length(name)) {
         | 0 => raise(Empty_identifier)
         | 1 => Stack.push(operandStack, Identifier(name))
         | _ =>
           /* turn multi-character identifiers into multiplication */
           let children =
             Array.map(name => Identifier(name), Js.String.split("", name));
           Stack.push(operandStack, Apply(Mul(true), children));
         }
       | Lexer.NUMBER(value) => Stack.push(operandStack, Number(value))
       | Lexer.RIGHT_PAREN =>
         let break = ref(false);
         while (! break^) {
           switch (Stack.pop(operatorStack)) {
           | Some((topOp, arity)) =>
             if (topOp == LeftParen) {
               break := true;
             } else {
               Stack.push(operandStack, Apply(topOp, popOperands(arity)));
             }
           | None => raise(Unmatched_right_paren)
           };
         };
       | Lexer.LEFT_PAREN =>
         switch (prevToken) {
         | Some(prevOp) =>
           if (prevOp == Lexer.RIGHT_PAREN) {
             /* handle implicit multiplication */
             switch (Stack.top(operatorStack)) {
             | Some((topOp, arity)) when topOp == Mul(true) =>
               replaceTop(operatorStack, (Mul(true), arity + 1))
             | _ => Stack.push(operatorStack, (Mul(true), 2))
             };
           }
         | _ => ()
         };
         Stack.push(operatorStack, (LeftParen, 0));
       | Lexer.PLUS => parseBinaryOp(~collate=true, Add)
       | Lexer.STAR => parseBinaryOp(~collate=true, Mul(false))
       | Lexer.MINUS => Stack.push(operatorStack, (Neg, 1))
       | Lexer.CARET => parseBinaryOp(Exp)
       | Lexer.EQUAL => parseBinaryOp(~collate=true, Eq)
       | _ => raise(Unknown_error)
       };
     });
  /* Clean up any operators that are still on the operator stack. */
  Stack.dynamicPopIter(
    operatorStack,
    ((op, arity)) => {
      if (op == LeftParen) {
        raise(Unmatched_left_paren);
      };
      Stack.push(operandStack, Apply(op, popOperands(arity)));
    },
  );
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

let opToString = (op: operator) =>
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
  };

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
  };

let sum = Array.fold_left((+.), 0.);

let prod = Array.fold_left(( *. ), 1.);

let rec evaluate = node =>
  switch (node) {
  | Apply(op, children) =>
    let children = Array.map(evaluate, children);
    switch (op) {
    | Add => children |> sum
    | Mul(_) => children |> prod
    | _ => 0.
    };
  | Identifier(_) => 0. /* allow option to provide a map of values */
  | Number(value) => float_of_string(value)
  };