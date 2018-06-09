open Node;

type transformer = {
  check: Node.t => bool,
  transform: Node.t => Node.t,
};

let isNumber = node =>
  switch (node) {
  | Number(_) => true
  | _ => false
  };

let isIdentifier = node =>
  switch (node) {
  | Identifier(_) => true
  | _ => false
  };

let rec isPolyTerm = node =>
  switch (node) {
  | Number(_) => true
  | Identifier(_) => true
  | Apply(Exp, [base, exp]) => isIdentifier(base) && isPolyTerm(exp)
  | Apply(Neg, [arg]) => isPolyTerm(arg)
  | Apply(Mul(_), args) => List.for_all(isPolyTerm, args)
  | _ => false
  };

exception NoCoefficient;

let rec getCoeff = node =>
  switch (node) {
  | Number(_) => node
  | Identifier(_) => Number("1")
  | Apply(Exp, _) => Number("1")
  | Apply(Neg, [arg]) => Apply(Neg, [getCoeff(arg)])
  | Apply(Mul(t), args) =>
    switch (List.filter(isNumber, args)) {
    | [] => Number("1")
    | [n] => n
    | nums => Apply(Mul(t), nums)
    }
  | _ => raise(NoCoefficient)
  };

let is = (a, b) => a == b;

let isNot = (a, b) => a != b;

let simplifyAddZero = {
  check: node =>
    switch (node) {
    | Apply(Add, children) => List.mem(Number("0"), children)
    | _ => false
    },
  transform: node =>
    switch (node) {
    | Apply(Add, children) =>
      switch (List.filter(isNot(Number("0")), children)) {
      | [] => Number("0")
      | [arg] => arg
      | args => Apply(Add, args)
      }
    | _ => node
    },
};

let simplifyMulOne = {
  check: node =>
    switch (node) {
    | Apply(Mul(_), children) => List.mem(Number("1"), children)
    | _ => false
    },
  transform: node =>
    switch (node) {
    | Apply(Mul(t), children) =>
      switch (List.filter(isNot(Number("1")), children)) {
      | [] => Number("1")
      | [arg] => arg
      | args => Apply(Mul(t), args)
      }
    | _ => node
    },
};

let simplifyDivOne = {
  check: node =>
    switch (node) {
    | Apply(Div, [_, Number("1")]) => true
    | _ => false
    },
  transform: node =>
    switch (node) {
    | Apply(Div, children) =>
      switch (children) {
      | [n, Number("1")] => n
      | _ => node
      }
    | _ => node
    },
};

let simplifyMulZero = {
  check: node =>
    switch (node) {
    | Apply(Mul(_), children) => List.mem(Number("0"), children)
    | _ => false
    },
  transform: node =>
    switch (node) {
    | Apply(Mul(_), children) =>
      List.mem(Number("0"), children) ? Number("0") : node
    | _ => node
    },
};