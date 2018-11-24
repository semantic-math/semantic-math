open Node;
open UniqueId;
type transformer = {
  check: Node.node => bool,
  transform: Node.node => Node.node,
};

let isNumber = ((_, typ)) =>
  switch (typ) {
  | Number(_) => true
  | _ => false
  };

let isIdentifier = ((_, typ)) =>
  switch (typ) {
  | Identifier(_) => true
  | _ => false
  };

let rec isPolyTerm = ((_, typ)) =>
  switch (typ) {
  | Number(_) => true
  | Identifier(_) => true
  | Apply(Exp, [base, exp]) => isIdentifier(base) && isPolyTerm(exp)
  | Apply(Neg, [arg]) => isPolyTerm(arg)
  | Apply(Mul(_), args) => List.for_all(isPolyTerm, args)
  | _ => false
  };

exception NoCoefficient;

let rec getCoeff = ((id, typ)) =>
  switch (typ) {
  | Number(_) => (id, typ)
  | Identifier(_) => (genId(), Number("1"))
  | Apply(Exp, _) => (genId(), Number("1"))
  | Apply(Neg, [arg]) => (genId(), Apply(Neg, [getCoeff(arg)]))
  | Apply(Mul(t), args) =>
    switch (List.filter(isNumber, args)) {
    | [] => (genId(), Number("1"))
    | [n] => n
    | nums => (genId(), Apply(Mul(t), nums))
    }
  | _ => raise(NoCoefficient)
  };

let is = (a, b) => a == b;

/* let isNot = (a, b) => a != b; */

let simplifyAddZero = {
  check: ((_, typ)) =>
    switch (typ) {
    | Apply(Add, children) =>
      List.exists(((_, typ)) => typ == Number("0"), children)
    | _ => false
    },
  transform: ((id, typ)) =>
    switch (typ) {
    | Apply(Add, children) =>
      switch (List.filter(((_, typ)) => typ != Number("0"), children)) {
      | [] => (genId(), Number("0"))
      | [arg] => arg
      | args => (genId(), Apply(Add, args))
      }
    | _ => (id, typ)
    },
};

let simplifyMulOne = {
  check: ((_, typ)) =>
    switch (typ) {
    | Apply(Mul(_), children) =>
      List.exists(((_, typ)) => typ == Number("1"), children)
    | _ => false
    },
  transform: ((id, typ)) =>
    switch (typ) {
    | Apply(Mul(t), children) =>
      switch (List.filter(((_, typ)) => typ != Number("1"), children)) {
      | [] => (genId(), Number("1"))
      | [arg] => arg
      | args => (genId(), Apply(Mul(t), args))
      }
    | _ => (id, typ)
    },
};

let simplifyDivOne = {
  check: ((_, typ)) =>
    switch (typ) {
    | Apply(Div, [_, (_, Number("1"))]) => true
    | _ => false
    },
  transform: ((id, typ)) =>
    switch (typ) {
    | Apply(Div, children) =>
      switch (children) {
      | [n, (_, Number("1"))] => n
      | _ => (id, typ)
      }
    | _ => (id, typ)
    },
};

let simplifyMulZero = {
  check: ((_, typ)) =>
    switch (typ) {
    | Apply(Mul(_), children) =>
      List.exists(((_, typ)) => typ == Number("0"), children)
    | _ => false
    },
  transform: ((id, typ)) =>
    switch (typ) {
    | Apply(Mul(_), children) =>
      List.exists(((_, typ)) => typ == Number("0"), children) ?
        (genId(), Number("0")) : (id, typ)
    | _ => (id, typ)
    },
};