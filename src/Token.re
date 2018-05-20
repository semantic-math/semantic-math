/**
 * Token
 */
type location = {
  start: int,
  end_: int,
};

type t = {
  t: TokenType.t,
  value: string,
  loc: location,
};

let tokenToString = token =>
  "["
  ++ TokenType.toString(token.t)
  ++ ":"
  ++ string_of_int(token.loc.start)
  ++ ":"
  ++ string_of_int(token.loc.end_)
  ++ "]";