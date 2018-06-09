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
  "sqrt",
  "log",
];

let funcForName = name => 
  switch (name) {
  | "cos" => Some(Js.Math.cos)
  | "sin" => Some(Js.Math.sin)
  | "tan" => Some(Js.Math.cos)
  | "sec" => Some(arg => 1. /. Js.Math.cos(arg))
  | "csc" => Some(arg => 1. /. Js.Math.sin(arg))
  | "cot" => Some(arg => 1. /. Js.Math.tan(arg))
  | "sqrt" => Some(Js.Math.sqrt)
  | "ln" => Some(Js.Math.log)     /* natural log */
  | "log" => Some(Js.Math.log10)
  | _ => None
  };

let valueForName = name =>
  switch (name) {
  | "pi" => Some(Js.Math._PI)
  | "tau" => Some(2. *. Js.Math._PI)
  | "e" => Some(Js.Math._E)
  | _ => None
  };

/* TODO(kevinb) figure out how to store the arity of these functions */
type well_known_funcs = 
  | Sin
  | Cos
  | Tan;
