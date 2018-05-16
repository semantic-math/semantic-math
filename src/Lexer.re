type token_type =
  | PLUS
  | MINUS
  | STAR
  | SPACE
  | SLASH
  | CARET
  | EQUAL
  | GREATER_THAN
  | GREATER_THAN_OR_EQUAL
  | LESS_THAN
  | LESS_THAN_OR_EQUAL
  | LEFT_PAREN
  | RIGHT_PAREN
  | COMMA
  | UNDERSCORE
  | ELLIPSES
  | BANG
  | SINGLE_QUOTE
  | IDENTIFIER(string)
  | NUMBER(string)
  | EOF;

type location = {
  start: int,
  end_: int,
};

type token = {
  t: token_type,
  value: string,
  loc: location,
};

let tokenTypeToString = tokenType =>
  switch (tokenType) {
  | PLUS => "+"
  | MINUS => "-"
  | STAR => "*"
  | SPACE => " " /* used for implicit multiplication */
  | SLASH => "/"
  | CARET => "^"
  | EQUAL => "="
  | GREATER_THAN => ">"
  | GREATER_THAN_OR_EQUAL => ">="
  | LESS_THAN => "<"
  | LESS_THAN_OR_EQUAL => "<="
  | LEFT_PAREN => "("
  | RIGHT_PAREN => ")"
  | COMMA => ","
  | UNDERSCORE => "_"
  | ELLIPSES => "..."
  | BANG => "!"
  | SINGLE_QUOTE => "'"
  | IDENTIFIER(name) => {j|IDENTIFIER($name)|j}
  | NUMBER(value) => {j|NUMBER($value)|j}
  | EOF => "EOF"
  };

let tokenToString = token =>
  "[" ++ tokenTypeToString(token.t) ++ ":" ++ string_of_int(token.loc.start) ++ ":" ++ string_of_int(token.loc.end_) ++ "]";

let idSubRe = "[a-zA-Z][a-zA-Z0-9]*";

let opSubRe = "\\.\\.\\.|<=|>=|!=|[\\<\\>\\!\\=\\(\\)\\+\\-\\/\\*\\^\\<\\>|\\,\\#\\_\\']";

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

let getAllResults = (string, regex) => {
  let continue = ref(true);
  let results = [||];
  while (continue^) {
    switch (Js.Re.exec(string, regex)) {
    | Some(result) => results |> Js.Array.push(result) |> ignore
    | None => continue := false
    };
  };
  results;
};

let groupsToTokenType = groups =>
  switch (groups) {
  | [Some(value), _, _] => Some(IDENTIFIER(value))
  | [_, _, Some(value)] => Some(NUMBER(value))
  | [_, Some(value), _] =>
    switch (value) {
    | "+" => Some(PLUS)
    | "*" => Some(STAR)
    | "-" => Some(MINUS)
    | "/" => Some(SLASH)
    | "^" => Some(CARET)
    | "=" => Some(EQUAL)
    | "(" => Some(LEFT_PAREN)
    | ")" => Some(RIGHT_PAREN)
    | "," => Some(COMMA)
    | "_" => Some(UNDERSCORE)
    | "<" => Some(LESS_THAN)
    | ">" => Some(GREATER_THAN)
    | "<=" => Some(LESS_THAN_OR_EQUAL)
    | ">=" => Some(GREATER_THAN_OR_EQUAL)
    | "..." => Some(ELLIPSES)
    | "!" => Some(BANG)
    | "'" => Some(SINGLE_QUOTE)
    | _ => None
    }
  | _ => None
  };

let lex = input : array(token) => {
  let tokens =
    getAllResults(input, regex)
    |> Array.map(result => {
         let capture =
           Js.Re.captures(result) |> Array.map(Js.Nullable.toOption);
         let index = Js.Re.index(result);
         switch (capture) {
         | [|Some(value), a, b, c|] => 
           switch(groupsToTokenType([a, b, c])) {
           | Some(t) => Some({
             t, 
             value, 
             loc: {
               start: index, 
               end_: index + String.length(value),
              },
            })
           | _ => None
           }
         | _ => None
         }
        })
    |. Belt.Array.keepMap(x => x);

  tokens |> Array.map(x => x.loc) |> ignore;
  tokens;
};