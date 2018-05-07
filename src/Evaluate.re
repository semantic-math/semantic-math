/**
 * Evaluate an AST.
 */
let sum = List.fold_left((+.), 0.);

let prod = List.fold_left(( *. ), 1.);

exception UnhandleFunction(Node.node);

exception UndefinedFunction(string);

exception UndefinedVariable(string);

exception UnhandledOperation(Node.operator);

let rec evaluate =
  Node.(
    node =>
      switch (node.node_desc) {
      | Apply(op, children) =>
        let children = List.map(evaluate, children);
        switch (op) {
        | Add => children |> sum
        | Mul(_) => children |> prod
        | Neg => (-1.) *. List.hd(children)
        | Div => List.nth(children, 0) /. List.nth(children, 1)
        | Exp => List.nth(children, 0) ** List.nth(children, 1)
        | Func(fn) =>
          /* TODO: handle functions wtih multiple args */
          /* TODO: handle user defined functions */
          let arg = List.hd(children);
          switch (fn.node_desc) {
          | Identifier(name) =>
            switch (Data.funcForName(name)) {
            | Some(fn) => fn(arg)
            | None => raise(UndefinedFunction(name))
            }
          | _ => raise(UnhandleFunction(fn))
          };
        | _ => raise(UnhandledOperation(op))
        };
      | Identifier(name) =>
        switch (Data.valueForName(name)) {
        | Some(value) => value
        | None => raise(UndefinedVariable(name))
        }
      | Number(value) => float_of_string(value)
      }
  );