let str = "a^2 * b^2 * c^2^x";

let tokens = Lexer.lex(str);

tokens |> Array.map(Token.tokenToString) |> Array.iter(Js.log);

let parser = Parser.make();
let result = Parser.parse(parser, tokens);

Js.log(Node.toString(result));

Js.log(Json.stringify(Node.toJson(result)));
