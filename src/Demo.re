let str = "a^2 * b^2 * c^2^x";

let tokens = Lexer.lex(str);

tokens |> Array.map(Lexer.tokenToString) |> Array.iter(Js.log);

let result = Parser.parse(tokens);

Js.log(Parser.nodeToString(result));