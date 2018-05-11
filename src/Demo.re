
let str = "a + b * c * d + e ^ -f ^ g";

let tokens = Lexer.lex(str);

tokens |> Array.map(Lexer.tokenToString) |> Array.iter(Js.log);

let result = Parser.parse(tokens, str);

Js.log(Parser.nodeToString(result));
