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
    | Eq => -1
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

exception Not_enough_operands;

exception Not_enough_operators;

exception Unknown_error;

exception Too_many_operands;

let popOperands = (stack, arity) => {
  let children = [||];
  for (_ in 1 to arity) {
    switch (Belt.MutableStack.pop(stack)) {
    | Some(value) => Js.Array.push(value, children) |> ignore
    | None => raise(Not_enough_operands)
    };
  };
  /* reverse the children so they're in the right order */
  Js.Array.reverseInPlace(children) |> ignore;
  children;
};

let last = arr =>
  switch (Array.length(arr)) {
  | 0 => None
  | n => Some(arr[n - 1])
  };

/* return a node */
let parse = (tokens: array(Lexer.token)) : node => {
  let operatorStack = Belt.MutableStack.make();
  let operandStack = Belt.MutableStack.make();
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
  /* TODO(kevinb): split this into 2-arity and n-arity cases */
  let parseAddOrMul = op =>
    switch (Belt.MutableStack.top(operatorStack)) {
    | Some((topOp, arity)) when op == topOp =>
      replaceTop(operatorStack, (op, arity + 1))
    | Some((topOp, arity)) when precedence(op) < precedence(topOp) =>
      let children = popOperands(operandStack, arity);
      Belt.MutableStack.pop(operatorStack) |> ignore;
      Belt.MutableStack.push(operandStack, `Apply((topOp, children)));
      /* case where the revealed operator matches the new operator */
      switch (Belt.MutableStack.top(operatorStack)) {
      | Some((topOp, arity)) when topOp == op =>
        replaceTop(operatorStack, (op, arity + 1))
      | _ => Belt.MutableStack.push(operatorStack, (op, 2))
      };
    | _ => Belt.MutableStack.push(operatorStack, (op, 2))
    };
  let parseOperator = op =>
    switch (op) {
    | Lexer.RIGHT_PAREN =>
      let break = ref(false);
      while (! break^) {
        switch (Belt.MutableStack.pop(operatorStack)) {
        | Some((topOp, arity)) =>
          if (topOp == Lexer.LEFT_PAREN) {
            break := true;
          } else {
            let children = popOperands(operandStack, arity);
            Belt.MutableStack.push(operandStack, `Apply((topOp, children)));
          }
        | None => raise(Not_enough_operators)
        };
      };
    | Lexer.LEFT_PAREN => Belt.MutableStack.push(operatorStack, (op, 0))
    | Lexer.Add => parseAddOrMul(op)
    | Lexer.Mul => parseAddOrMul(op)
    | Lexer.Neg => Belt.MutableStack.push(operatorStack, (Lexer.Neg, 1))
    | Lexer.Exp => Belt.MutableStack.push(operatorStack, (Lexer.Exp, 2))
    | Lexer.Eq => parseAddOrMul(op)
    | _ => raise(Unknown_error)
    };
  /* Process each token. */
  tokens
  |> Array.iter(token =>
       switch (token) {
       | `Operator(op) => parseOperator(op)
       | `Identifier(name) =>
         Belt.MutableStack.push(operandStack, `Identifier(name))
       | `Number(value) =>
         Belt.MutableStack.push(operandStack, `Number(value))
       }
     );
  /* Clean up any operators that are still on the operator stack. */
  Belt.MutableStack.dynamicPopIter(
    operatorStack,
    ((op, arity)) => {
      let children = popOperands(operandStack, arity);
      Belt.MutableStack.push(operandStack, `Apply((op, children)));
    },
  );
  /* Check if we have a single value left and return that value in that case. */
  switch (Belt.MutableStack.size(operandStack)) {
  | 0 => raise(Not_enough_operands)
  | 1 =>
    switch (Belt.MutableStack.pop(operandStack)) {
    | Some(value) => value
    | None => raise(Unknown_error)
    }
  | _ => raise(Too_many_operands)
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