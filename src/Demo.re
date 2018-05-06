Js.log("re-math-parser demo");

/* let str = "sin^-1 (x)"; */
let str = "a^-2b";
let tokens = Lexer.lex(str);
tokens |> Array.map(Lexer.tokenToString) |> Array.iter(Js.log);

let ast = Parser.parse(tokens, str);
Js.log(Parser.nodeToString(ast));

let result = Evaluate.evaluate(ast);
Js.log(result);
