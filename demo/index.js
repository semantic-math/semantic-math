import * as Node from "../lib/es6_global/packages/ast/src/Node.bs.js";
import * as Lexer from "../lib/es6_global/packages/parser/src/Lexer.bs.js";
import * as MathParser from "../lib/es6_global/packages/parser/src/MathParser.bs.js";

const input = document.querySelector("#input");
const output = document.querySelector("#output");

const math = "f(x) = 2x + 5";
input.value = math;

const tokens = Lexer.lex(math);
const ast = Node.toJson(MathParser.parse(tokens));

output.innerText = JSON.stringify(ast, null, 2);

input.addEventListener("input", () => {
    try {
        const tokens = Lexer.lex(input.value);
        const ast = Node.toJson(MathParser.parse(tokens));
        output.innerText = JSON.stringify(ast, null, 2);
    } catch (e) {
        output.innerText = e;
    }
});
