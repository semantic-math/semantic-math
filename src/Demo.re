Js.log("re-math-parser demo");

let str = "2 * (3 + 4)";
let tokens = Lexer.lex(str);
tokens |> Array.map(Lexer.printToken) |> Array.iter(Js.log);

let ast = Parser.parse(tokens);
Js.log(Parser.nodeToString(ast));

let result = Parser.evaluate(ast);
Js.log(result);
