Js.log("re-math-parser demo");

let str = "2 * (3 + 4)";
let tokens = Lexer.lex(str);
tokens |> Array.map(Lexer.tokenToString) |> Array.iter(Js.log);

let ast = Parser.parse(tokens);
let ast2 = Parser.transform(node => {
    switch (node.Parser.node_desc) {
    | Parser.Number(value) => 
        {
            node_desc: Parser.Number(string_of_int(2 * int_of_string(value))),
            Parser.loc: {
                Lexer.start: -1,
                Lexer.end_: -1,
            },
        }
    | _ => node
    }
}, ast);

Js.log(Parser.nodeToString(ast));
Js.log(Parser.nodeToString(ast2));

let result = Evaluate.evaluate(ast);
Js.log(result);
