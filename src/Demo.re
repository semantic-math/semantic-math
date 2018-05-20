let str = "a^2 * b^2 * c^2^x";

let tokens = Lexer.lex(str);

tokens |> Array.map(Token.tokenToString) |> Array.iter(Js.log);

let result = MathParser.parse(tokens);

Js.log(Node.toString(result));

Js.log(Json.stringify(Node.toJson(result)));
