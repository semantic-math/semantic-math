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
    testParser("a^2 * b^2 * c^2", "[* [^ a 2] [^ b 2] [^ c 2]]");  
  });
  describe("multiplication", () => {
    testParser("a * b * c", "[* a b c]");
    testParser("abc", "[* a b c]");
    testParser("abcd", "[* a b c d]");
    testParser("-ab - bc", "[+ [neg [* a b]] [neg [* b c]]]");
    testParser("ab * cd", "[* [* a b] [* c d]]");
    testParser("(a)(b)(c)", "[* a b c]");
    testParser("(a)(b) * (c)(d)", "[* [* a b] [* c d]]");
    testParser("-(x)(y)(z)", "[neg [* x y z]]");
    testParser("2x", "[* 2 x]");
    testParser("2(x)", "[* 2 x]");
    testParser("2(x)(y) * 3(a)(b)", "[* [* 2 x y] [* 3 a b]]");
    testParser("123xy", "[* 123 x y]");
    testParser("x^-1y^-1", "[* [^ x [neg 1]] [^ y [neg 1]]]");
    testParser("x^2y^2", "[* [^ x 2] [^ y 2]]");
    testParser("a^2b^3 * x^2y^3", "[* [* [^ a 2] [^ b 3]] [* [^ x 2] [^ y 3]]]");
    testParser("x^2*y^2", "[* [^ x 2] [^ y 2]]");
    testParser("x*y^2", "[* x [^ y 2]]");
    testParser("xy^2", "[* x [^ y 2]]");
  });
  describe("division", () => {
    testParser("1/2", "[/ 1 2]");
    testParser("1/2/3", "[/ [/ 1 2] 3]");
    testParser("1/xy", "[/ 1 [* x y]]");
    testParser("1/xy^2", "[/ 1 [* x [^ y 2]]]");
    testParser("1/x^2", "[/ 1 [^ x 2]]");
    testParser("a/b * 1/2", "[* [/ a b] [/ 1 2]]");
    testParser("ab / cd", "[/ [* a b] [* c d]]");
    testParser("a * b / c * d", "[* a [/ b c] d]");
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
    testParser("a^-2*b^-3", "[* [^ a [neg 2]] [^ b [neg 3]]]");
  });
  describe("equations", () => {
    testParser("x + 5 = 10", "[= [+ x 5] 10]");
    testParser("x = y = z", "[= x y z]");
  });
  describe("inequalities", () => {
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
  describe("function", () => {
    testParser("f(x)", "[f x]");
    testParser("f(x, y)", "[f x y]");
    testParser("f(g(x),y)", "[f [g x] y]");
    testParser("f(x) = 2*x + 5", "[= [f x] [+ [* 2 x] 5]]");
    testParser("sin(x)", "[sin x]");
    testParser("2sin(x)", "[sin x]"); /* We're not using the correct precedence for ( here */
    testParser("cos(x + pi/2)", "[cos [+ x [/ pi 2]]]");
    testParser("sin^2(x)", "[[^ sin 2] x]");
    testParser("sin^-1 (x)", "[[^ sin [neg 1]] x]");
  });
});