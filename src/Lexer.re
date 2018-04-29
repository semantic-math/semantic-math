type token = 
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | CARET
  | EQUAL
  | GREATER_THAN
  | LESS_THAN
  | LEFT_PAREN
  | RIGHT_PAREN
  | COMMA
  | IDENTIFIER(string)
  | NUMBER(string);

let tokenToString = (token) =>
  switch(token) {
  | PLUS => "+"
  | MINUS => "-"
  | STAR => "*"
  | SLASH => "/"
  | CARET => "^"
  | EQUAL => "="
  | GREATER_THAN => ">"
  | LESS_THAN => "<"
  | LEFT_PAREN => "("
  | RIGHT_PAREN => ")"
  | COMMA => ","
  | IDENTIFIER(name) => {j|IDENTIFIER($name)|j}
  | NUMBER(value) => {j|NUMBER($value)|j}
  };

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
         | [|_, Some(ident), _, _|] => Some(IDENTIFIER(ident))
         | [|_, _, Some(oper), _|] =>
           switch (oper) {
           | "+" => Some(PLUS)
           | "*" => Some(STAR)
           | "-" => Some(MINUS)
           | "/" => Some(SLASH)
           | "^" => Some(CARET)
           | "=" => Some(EQUAL)
           | "(" => Some(LEFT_PAREN)
           | ")" => Some(RIGHT_PAREN)
           | "," => Some(COMMA)
           | _ => None
           }
         | [|_, _, _, Some(num)|] => Some(NUMBER(num))
         | _ => None
         }
       )
    |. Belt.Array.keepMap(x => x);
  tokens;
};