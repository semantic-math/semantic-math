open Jest;

describe("Parser", () => {
  open Expect;
  let testParser = (expr, tree) =>
    test(
      expr ++ " parses as " ++ tree,
      () => {
        let tokens = Lexer.lex(expr);
        let ast = OldParser.parse(tokens, expr);
        expect(OldParser.nodeToString(ast)) |> toBe(tree);
      },
    );
  /* let testError = (expr, exc) =>
    test(expr ++ " raises " ++ Printexc.to_string(exc), () =>
      expect(() =>
        Parser.parse(Lexer.lex(expr), expr)
      ) |> toThrowException(exc)
    ); */
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
    testParser("2x", "[* 2 x]");
    testParser("2(x)", "[* 2 x]");
    testParser("123xy", "[* 123 x y]");
    testParser("x^2*y^2", "[* [^ x 2] [^ y 2]]");
    testParser("x*y^2", "[* x [^ y 2]]");
    testParser("xy^2", "[* x [^ y 2]]");
  });
  describe("division", () => {
    testParser("1/2", "[/ 1 2]");
    testParser("1/2/3", "[/ [/ 1 2] 3]");
    testParser("1/xy", "[/ 1 [* x y]]");
    testParser("1/x^2", "[/ 1 [^ x 2]]");
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
    testParser("1-2+3", "[+ 1 [neg 2] 3]");
    testParser("1--2", "[+ 1 [neg [neg 2]]]");
    testParser("-1", "[neg 1]");
    testParser("--1", "[neg [neg 1]]");
    testParser("1 - (2 * 3)", "[+ 1 [neg [* 2 3]]]");
    testParser("-(2 * 3)", "[neg [* 2 3]]");
    testParser("-2 * 3", "[* [neg 2] 3]");
    testParser("(2 * 3) - 4", "[+ [* 2 3] [neg 4]]");
  });
  describe("exponents", () => {
    testParser("2^3", "[^ 2 3]");
    testParser("2^3^4", "[^ [^ 2 3] 4]");
    testParser("-2^x", "[neg [^ 2 x]]");
    testParser("(-2)^x", "[^ [neg 2] x]");
    testParser("a^-2b^-3", "[* [^ a [neg 2]] [^ b [neg 3]]]");
  });
  Skip.describe("equations", () => {
    testParser("x + 5 = 10", "[= [+ x 5] 10]");
    testParser("x = y = z", "[= x y z]");
  });
  Skip.describe("inequalities", () => {
    testParser("x < y", "[< x y]");
    testParser("x < y < z", "[< x y z]");
    testParser("x <= y", "[<= x y]");
    testParser("x <= y <= z", "[<= x y z]");
    testParser("x > y", "[> x y]");
    testParser("x > y > z", "[> x y z]");
    testParser("x >= y", "[>= x y]");
    testParser("x >= y >= z", "[>= x y z]");
    /* What should this case parse as */
    /* testParser("a > b < c", ""); */
  });
  /* describe("errors", () => {
    testError("(x+1", Parser.Unmatched_left_paren);
    testError("x+1)", Parser.Unmatched_right_paren);
    testError("1 + 2 3", Parser.Missing_operator);
    testError("1 + 2 + +", Parser.Missing_operand);
  }); */
  Skip.describe("function", () => {
    testParser("f(x)", "[f x]");
    testParser("f(x, y)", "[f x y]");
    testParser("f(g(x),y)", "[f [g x] y]");
    testParser("f(x) = 2*x + 5", "[= [f x] [+ [* 2 x] 5]]");
    testParser("sin(x)", "[sin x]");
    testParser("cos(x + pi/2)", "[cos [+ x [/ pi 2]]]");
    testParser("sin^2(x)", "[[^ sin 2] x]");
    testParser("sin^-1 (x)", "[[^ sin [neg 1]] x]");
  });
});