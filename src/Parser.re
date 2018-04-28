let printToken = token =>
  switch (token) {
  | `Operator(op) =>
    switch (op) {
    | Lexer.Add => "add"
    | Lexer.Sub => "sub"
    | Lexer.Div => "div"
    | Lexer.Mul => "mul"
    | Lexer.Neg => "neg"
    | Lexer.Exp => "exp"
    | Lexer.Eq => "eq"
    | Lexer.LEFT_PAREN => "LEFT_PAREN"
    | Lexer.RIGHT_PAREN => "RIGHT_PAREN"
    }
  | `Identifier(name) => {j|identifier = $name|j}
  | `Number(value) => {j|number = $value|j}
  | `End => "end"
  };

type node = [
  | `Apply(Lexer.operator, array(node))
  | `Identifier(string)
  | `Number(string)
];

let precedence = op =>
  Lexer.(
    switch (op) {
    | Eq => (-1)
    | LEFT_PAREN => 0
    | RIGHT_PAREN => 0
    | Add => 1
    | Sub => 1
    | Mul => 2
    | Div => 2
    | Neg => 3
    | Exp => 4
    }
  );

let replaceTop = (stack, value) => {
  Belt.MutableStack.pop(stack) |> ignore;
  Belt.MutableStack.push(stack, value);
};

exception Missing_operator;

exception Missing_operand;

exception Unmatched_left_paren;

exception Unmatched_right_paren;

exception Unknown_error;

exception Empty_identifier;

let last = arr =>
  switch (Array.length(arr)) {
  | 0 => None
  | n => Some(arr[n - 1])
  };

module Stack = Belt.MutableStack;

/* return a node */
let parse = (tokens: array(Lexer.token)) : node => {
  let operatorStack = Stack.make();
  let operandStack = Stack.make();
  let popOperands = arity => {
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
        | `Operator(Lexer.Sub) =>
          switch (last(accum)) {
          | Some(token)
              when
                token != `Operator(Lexer.Neg)
                && token != `Operator(Lexer.LEFT_PAREN) =>
            Js.Array.push(`Operator(Lexer.Add), accum) |> ignore
          | _ => ()
          };
          Js.Array.push(`Operator(Lexer.Neg), accum) |> ignore;
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
      Stack.push(operandStack, `Apply((topOp, popOperands(arity))));
      /* case where the revealed operator matches the new operator */
      switch (Stack.top(operatorStack)) {
      | Some((topOp, arity)) when topOp == op =>
        replaceTop(operatorStack, (op, arity + 1))
      | _ => Stack.push(operatorStack, (op, 2))
      };
    | _ => Stack.push(operatorStack, (op, 2))
    };
  let parseOperator = (op, prevToken) =>
    switch (op) {
    | Lexer.RIGHT_PAREN =>
      let break = ref(false);
      while (! break^) {
        switch (Stack.pop(operatorStack)) {
        | Some((topOp, arity)) =>
          if (topOp == Lexer.LEFT_PAREN) {
            break := true;
          } else {
            Stack.push(operandStack, `Apply((topOp, popOperands(arity))));
          }
        | None => raise(Unmatched_right_paren)
        };
      };
    | Lexer.LEFT_PAREN =>
      switch (prevToken) {
      | Some(`Operator(prevOp)) =>
        if (prevOp == Lexer.RIGHT_PAREN) {
          /* handle implicit multiplication */
          switch (Stack.top(operatorStack)) {
          | Some((topOp, arity)) when Lexer.Mul == topOp =>
            replaceTop(operatorStack, (Lexer.Mul, arity + 1))
          | _ => Stack.push(operatorStack, (Lexer.Mul, 2))
          };
        }
      | _ => ()
      };
      Stack.push(operatorStack, (op, 0));
    | Lexer.Add => parseBinaryOp(~collate=true, op)
    | Lexer.Mul => parseBinaryOp(~collate=true, op)
    | Lexer.Neg => Stack.push(operatorStack, (Lexer.Neg, 1))
    | Lexer.Exp => parseBinaryOp(op)
    | Lexer.Eq => parseBinaryOp(~collate=true, op)
    | _ => raise(Unknown_error)
    };
  /* Process each token. */
  tokens
  |> Array.iteri((i, token) => {
       let lastToken = i > 0 ? Some(tokens[i - 1]) : None;
       switch (token) {
       | `Operator(op) => parseOperator(op, lastToken)
       | `Identifier(name) =>
         switch (String.length(name)) {
         | 0 => raise(Empty_identifier)
         | 1 => Stack.push(operandStack, `Identifier(name))
         | _ =>
           /* turn multi-character identifiers into multiplication */
           let children =
             Array.map(
               name => `Identifier(name),
               Js.String.split("", name),
             );
           Stack.push(operandStack, `Apply((Lexer.Mul, children)));
         }
       | `Number(value) => Stack.push(operandStack, `Number(value))
       };
     });
  /* Clean up any operators that are still on the operator stack. */
  Stack.dynamicPopIter(
    operatorStack,
    ((op, arity)) => {
      if (op == Lexer.LEFT_PAREN) {
        raise(Unmatched_left_paren);
      };
      Stack.push(operandStack, `Apply((op, popOperands(arity))));
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

let opToString = (op: Lexer.operator) =>
  switch (op) {
  | Add => "+"
  | Sub => "-"
  | Mul => "*"
  | Div => "/"
  | Neg => "neg"
  | Exp => "^"
  | Eq => "="
  | LEFT_PAREN => "("
  | RIGHT_PAREN => ")"
  };

let rec nodeToString = node =>
  switch (node) {
  | `Apply(op, children) =>
    "["
    ++ Js.Array.joinWith(
         " ",
         Array.append(
           [|opToString(op)|],
           Array.map(nodeToString, children),
         ),
       )
    ++ "]"
  | `Identifier(name) => name
  | `Number(value) => value
  };

let sum = Array.fold_left((+.), 0.);

let prod = Array.fold_left(( *. ), 1.);

let rec evaluate = node =>
  switch (node) {
  | `Apply(op, children) =>
    let children = Array.map(evaluate, children);
    switch (op) {
    | Lexer.Add => children |> sum
    | Lexer.Mul => children |> prod
    | _ => 0.
    };
  | `Identifier(_) => 0. /* allow option to provide a map of values */
  | `Number(value) => float_of_string(value)
  };