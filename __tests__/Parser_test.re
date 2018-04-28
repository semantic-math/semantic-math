open Jest;

describe("Parser", () => {
  open Expect;
  let testParser = (expr, tree) =>
    test(
      expr ++ " parses as " ++ tree,
      () => {
        let tokens = Lexer.lex(expr);
        let ast = Parser.parse(tokens);
        expect(Parser.nodeToString(ast)) |> toBe(tree);
      },
    );
  describe("order of operations", () => {
    testParser("1+2+3", "[+ 1 2 3]");
    testParser("1+2*3+4*5+6", "[+ 1 [* 2 3] [* 4 5] 6]");
  });
  describe("parentheses", () => {
    testParser("2*(3+4)", "[* 2 [+ 3 4]]");
    testParser("(1+(2+(3+4)))", "[+ 1 [+ 2 [+ 3 4]]]");
    testParser("(3+4)", "[+ 3 4]");
    testParser("((3+4))", "[+ 3 4]");
    testParser("(2)", "2");
    testParser("(-2)", "[neg 2]");
  });
  describe("subtraction/negation", () => {
    testParser("1-2", "[+ 1 [neg 2]]");
    testParser("1--2", "[+ 1 [neg [neg 2]]]");
    testParser("-1", "[neg 1]");
    testParser("--1", "[neg [neg 1]]");
    testParser("1 - (2 * 3)", "[+ 1 [neg [* 2 3]]]");
    testParser("-(2 * 3)", "[neg [* 2 3]]");
    testParser("-2 * 3", "[* [neg 2] 3]");
  });
  describe("exponents", () => {
    testParser("2^3", "[^ 2 3]");
    testParser("2^3^4", "[^ 2 [^ 3 4]]");
    testParser("-2^x", "[neg [^ 2 x]]");
    testParser("(-2)^x", "[^ [neg 2] x]");
  });
  describe("equations", () => {
    testParser("x + 5 = 10", "[= [+ x 5] 10]");
  });
});