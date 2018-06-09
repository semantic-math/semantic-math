type t =
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
  | IDENTIFIER
  | NUMBER
  | EOF;


let toString = tokenType =>
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
  | IDENTIFIER => "IDENTIFIER"
  | NUMBER => "NUMBER"
  | EOF => "EOF"
  };

let compare = (x: t, y: t) =>
  Pervasives.compare(toString(x), toString(y));
