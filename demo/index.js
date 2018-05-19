const input = document.querySelector("#input");
const output = document.querySelector("#output");

const math = "f(x) = 2x + 5";
input.value = math;

const tokens = Lexer.lex(math);
const ast = Parser.nodeToJson(Parser.parse(tokens));

output.innerText = JSON.stringify(ast, null, 2);

input.addEventListener("input", () => {
    try {
        const tokens = Lexer.lex(input.value);
        const ast = Parser.nodeToJson(Parser.parse(tokens));
        output.innerText = JSON.stringify(ast, null, 2);
    } catch (e) {
        output.innerText = e;
    }
});
