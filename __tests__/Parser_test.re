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
  let testError = (expr, exc) =>
    test(expr ++ " raises " ++ Printexc.to_string(exc), () =>
      expect(() =>
        Parser.parse(Lexer.lex(expr))
      ) |> toThrowException(exc)
    );
  describe("order of operations", () => {
    testParser("1+2+3", "[+ 1 2 3]");
    testParser("1+2*3+4*5+6", "[+ 1 [* 2 3] [* 4 5] 6]");
  });
  describe("multiplication", () => {
    testParser("abc", "[* a b c]");
    testParser("abcd", "[* a b c d]");
    testParser("ab * cd", "[* [* a b] [* c d]]");
    testParser("(a)(b)(c)", "[* a b c]");
    testParser("(a)(b) * (c)(d)", "[* [* a b] [* c d]]");
    /* testParser("2x", "[* 2 x]");
    testParser("2xy", "[* 2 x y]"); */
    testParser("x^2*y^2", "[* [^ x 2] [^ y 2]]");
    testParser("x*y^2", "[* x [^ y 2]]");
    testParser("xy^2", "[* x [^ y 2]]");
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
    testParser("x = y = z", "[= x y z]");
  });
  describe("errors", () => {
    testError("(x+1", Parser.Unmatched_left_paren);
    testError("x+1)", Parser.Unmatched_right_paren);
    testError("1 + 2 3", Parser.Missing_operator);
    testError("1 + 2 + +", Parser.Missing_operand);
  });
  describe("function", () => {
    testParser("f(x)", "[f x]");
    testParser("f(x, y)", "[f x y]");
    testParser("f(g(x),y)", "[f [g x] y]");
    testParser("f(x) = 2*x + 5", "[= [f x] [+ [* 2 x] 5]]");
  });
});