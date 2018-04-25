let printToken = token =>
  switch (token) {
  | `Operator(op) =>
    switch (op) {
    | Lexer.Add => "add"
    | Lexer.Sub => "sub"
    | Lexer.Div => "div"
    | Lexer.Mul => "mul"
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
    | Add
    | Sub => 1
    | Mul
    | Div => 2
    }
  );

let replaceTop = (stack, value) => {
  Belt.MutableStack.pop(stack) |> ignore;
  Belt.MutableStack.push(stack, value);
};

exception Not_enough_operands;

exception Not_enough_operators;

exception Unknown_error;

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

/* return a node */
let parse = tokens : node => {
  let operatorStack = Belt.MutableStack.make();
  let operandStack = Belt.MutableStack.make();

  let parseOperator = op =>
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
    | Some(_) => Belt.MutableStack.push(operatorStack, (op, 2))
    | None => Belt.MutableStack.push(operatorStack, (op, 2))
    };

  tokens
  |> Array.iter(token =>
       switch (token) {
       | `Operator(op) => parseOperator(op)
       | `Identifier(name) =>
         Belt.MutableStack.push(operandStack, `Identifier(name))
       | `Number(value) =>
         Belt.MutableStack.push(operandStack, `Number(value))
       | `End =>
         switch (Belt.MutableStack.pop(operatorStack)) {
         | Some((op, arity)) =>
           let children = popOperands(operandStack, arity);
           Belt.MutableStack.push(operandStack, `Apply((op, children)));
         | None => raise(Not_enough_operators)
         }
       }
     );

  switch (Belt.MutableStack.pop(operandStack)) {
  | Some(value) => value
  | None => raise(Unknown_error)
  };
};

let opToString = (op: Lexer.operator) =>
  switch (op) {
  | Add => "+"
  | Sub => "-"
  | Mul => "*"
  | Div => "/"
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
