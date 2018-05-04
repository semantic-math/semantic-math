Js.log("re-math-parser demo");

let str = "2 * (3 + 4)";
let tokens = Lexer.lex(str);
tokens |> Array.map(Lexer.tokenToString) |> Array.iter(Js.log);

let ast = Parser.parse(tokens |> Array.map(x => x.Lexer.t));
Js.log(Parser.nodeToString(ast));

let result = Evaluate.evaluate(ast);
Js.log(result);
