type location = {
    loc_start: int,
    loc_end: int,
};

let x = 5 and y = 10 and z = 15;
type a = string and b = int and c = float;

type node_desc = 
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
and node = {
  desc: node_desc,
  loc: location,
};

let mk_loc = (loc_start, loc_end) => {loc_start, loc_end};

let mk_num = (num, loc) => {desc: Number(num), loc};

let mk_add = (children, loc) => {desc: Apply(Add, children), loc};

let ast = mk_add(
    [|
        mk_num("5", mk_loc(5, 6)),
        mk_num("7", mk_loc(7, 8)),
    |], 
    mk_loc(5, 8),  /* location of the whole node, e.g. 5+7 */
);

/* The operators themsevlves should have locations too */

/* let n1: node = mk_num("123", mk_loc(5, 10)); */

/* let n1: node = {
    desc: Apply(Add, [||]),
    loc: mk_loc(5, 10),
}; */
