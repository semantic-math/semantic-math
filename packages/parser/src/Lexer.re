/**
 * Lexer
 */
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
  TokenType.(
    switch (groups) {
    | [Some(_), _, _] => Some(IDENTIFIER)
    | [_, _, Some(_)] => Some(NUMBER)
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
    }
  );

let lex = input : array(Token.t) => {
  open Token;
  let tokens =
    getAllResults(input, regex)
    |> Array.map(result => {
         let capture =
           Js.Re.captures(result) |> Array.map(Js.Nullable.toOption);
         let index = Js.Re.index(result);
         switch (capture) {
         | [|Some(value), a, b, c|] =>
           switch (groupsToTokenType([a, b, c])) {
           | Some(t) =>
             Some({
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
         };
       })
    |. Belt.Array.keepMap(x => x);
  tokens |> Array.map(x => x.loc) |> ignore;
  tokens;
};