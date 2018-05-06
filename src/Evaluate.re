let sum = List.fold_left((+.), 0.);
let prod = List.fold_left(( *. ), 1.);

let rec evaluate =
  Parser.(
    node =>
      switch (node.node_desc) {
      | Apply(op, children) =>
        let children = List.map(evaluate, children);
        switch (op) {
        | Add => children |> sum
        | Mul(_) => children |> prod
        | _ => 0.
        };
      | Identifier(_) => 0. /* allow option to provide a map of values */
      | Number(value) => float_of_string(value)
      }
  );
  