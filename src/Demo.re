Js.log("re-math-parser demo");

let tokens = Lexer.lex("1 + 2*3 + 4*5 + 6");
/* tokens |> Array.map(Parser.printToken) |> Array.iter(Js.log); */

let ast = Parser.parse(tokens);
Js.log(Parser.nodeToString(ast));

let result = Parser.evaluate(ast);
Js.log(result);
