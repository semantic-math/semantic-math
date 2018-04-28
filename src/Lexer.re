type operator =
  | Add
  | Sub
  | Mul
  | Div
  | Neg
  | Exp
  | Eq
  | LEFT_PAREN
  | RIGHT_PAREN;

type token = [
  | `Operator(operator)
  | `Identifier(string)
  | `Number(string)
];

let idSubRe = "[a-zA-Z][a-zA-Z0-9]*";
let opSubRe = "<=|>=|!=|[\\<\\>\\!\\=\\(\\)\\+\\-\\/\\*\\^\\<\\>|\\,\\#\\_]";
let numSubRe = "\\d*\\.\\d+|\\d+\\.\\d*|\\d+";

let regex =
  Js.Re.fromStringWithFlags(
    {j|($idSubRe)|($opSubRe)|($numSubRe)|j},
    ~flags="g",
  );

let getAllCaptures = (string, regex) => {
  let continue = ref(true);
  let allCaptures = [||];
  while (continue^) {
    switch (Js.Re.exec(string, regex)) {
    | Some(result) =>
      Js.Array.push(
        Array.map(Js.Nullable.toOption, Js.Re.captures(result)),
        allCaptures,
      )
      |> ignore
    | None => continue := false
    };
  };
  allCaptures;
};

let lex = input : array(token) => {
  let tokens =
    getAllCaptures(input, regex)
    |> Array.map(capture =>
         switch (capture) {
         | [|_, Some(ident), _, _|] => Some(`Identifier(ident))
         | [|_, _, Some(oper), _|] =>
           switch (oper) {
           | "+" => Some(`Operator(Add))
           | "*" => Some(`Operator(Mul))
           | "-" => Some(`Operator(Sub))
           | "/" => Some(`Operator(Div))
           | "^" => Some(`Operator(Exp))
           | "=" => Some(`Operator(Eq))
           | "(" => Some(`Operator(LEFT_PAREN))
           | ")" => Some(`Operator(RIGHT_PAREN))
           | _ => None
           }
         | [|_, _, _, Some(num)|] => Some(`Number(num))
         | _ => None
         }
       )
    |. Belt.Array.keepMap(x => x);
  tokens;
};