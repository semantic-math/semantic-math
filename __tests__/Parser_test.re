open Jest;
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

describe("Parser", () => {
  describe("order of operations", () => {
    testParser("1+2+3", "[+ 1 2 3]");
    testParser("1+2*3+4*5+6", "[+ 1 [* 2 3] [* 4 5] 6]");
    testParser("a^2 * b^2 * c^2", "[* [^ a 2] [^ b 2] [^ c 2]]");  
  });
  describe("multiplication", () => {
    testParser("a * b * c", "[* a b c]");
    testParser("abc", "[* a b c]");
    testParser("abcd", "[* a b c d]");
    testParser("-ab - bc", "[+ [* [neg a] b] [neg [* b c]]]");
    testParser("-ab + -bc", "[+ [* [neg a] b] [* [neg b] c]]");
    testParser("ab * cd", "[* [* a b] [* c d]]");
    testParser("abc - xyz", "[+ [* a b c] [neg [* x y z]]]");
    testParser("(a)(b)(c)", "[* a b c]");
    testParser("(a)(b) * (c)(d)", "[* [* a b] [* c d]]");
    testParser("-(x)(y)(z)", "[* [neg x] y z]");
    testParser("2x", "[* 2 x]");
    testParser("-2xy", "[* [neg 2] x y]");
    testParser("1 - 2xy", "[+ 1 [neg [* 2 x y]]]");
    testParser("1 + -2xy", "[+ 1 [* [neg 2] x y]]");
    testParser("2(x)", "[* 2 x]");
    testParser("(2)(x)", "[* 2 x]");
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
  describe("function", () => {
    testParser("f(x)", "[f x]");
    testParser("f(x, y)", "[f x y]");
    testParser("f(g(x),y)", "[f [g x] y]");
    testParser("f(x) = 2*x + 5", "[= [f x] [+ [* 2 x] 5]]");
    testParser("sin(x)", "[sin x]");
    testParser("2 * sin(x)", "[* 2 [sin x]]");
    testParser("2 sin(x)", "[* 2 [sin x]]");
    testParser("2x sin(x)", "[* 2 x [sin x]]");
    testParser("cos(x + pi/2)", "[cos [+ x [/ pi 2]]]");
    testParser("sin^2(x)", "[[^ sin 2] x]");
    testParser("sin^-1 (x)", "[[^ sin [neg 1]] x]");
    testParser("log_n(x)", "[[_ log n] x]");
    testParser("log_10(x)", "[[_ log 10] x]");
  });
  describe("subscripts", () => {
    testParser("a_n", "[_ a n]");
    testParser("a_(n-1)", "[_ a [+ n [neg 1]]]");
    testParser("a_n_i", "[_ [_ a n] i]");
    testParser("a_n^2", "[^ [_ a n] 2]");
    testParser("a_n b_m", "[* [_ a n] [_ b m]]");
    testParser("a_n^-1", "[^ [_ a n] [neg 1]]");
    testParser("a_n^-2 b_m^-2", "[* [^ [_ a n] [neg 2]] [^ [_ b m] [neg 2]]]");
  });
  describe("ellipses", () => {
    testParser("a_0 + ... + a_n", "[+ [_ a 0] ... [_ a n]]");
    testParser("1 + 2 + ...", "[+ 1 2 ...]");
    testParser("a_0 * ... * a_n", "[* [_ a 0] ... [_ a n]]");
    testParser("1 * 2 * ...", "[* 1 2 ...]");
    testParser("a_0 = ... = a_n = ... = a_m", "[= [_ a 0] ... [_ a n] ... [_ a m]]");
    testParser("a_0a_1 ... a_n", "[* [_ a 0] [_ a 1] ... [_ a n]]");
  });
  describe("factorial", () => {
    testParser("0!", "[! 0]");
    testParser("n!", "[! n]");
    testParser("a_n!", "[! [_ a n]]");
    testParser("2n!", "[* 2 [! n]]");
    testParser("n!m!", "[* [! n] [! m]]");
    testParser("(n-1)!", "[! [+ n [neg 1]]]");
    testParser("n! * (n-1)!", "[* [! n] [! [+ n [neg 1]]]]");
    testParser("n!(n-1)!", "[* [! n] [! [+ n [neg 1]]]]");
    testParser("n!!", "[! [! n]]");
    testParser("n!m!/(n-1)!", "[/ [* [! n] [! m]] [! [+ n [neg 1]]]]");
  });
  describe("errors", () => {
    testError("(x+1", Parser.UnmatchedLeftParen);
    testError("(x+1)(y+2", Parser.UnmatchedLeftParen);
    testError("x+1)", Parser.UnmatchedRightParen);
    testError("x+1)(y+2)", Parser.UnmatchedRightParen);
    testError("1 + 2 3", Parser.UnexpectedToken);
    testError("1 + 2 + +", Parser.UnexpectedToken);
    testError("1 * * 2", Parser.UnexpectedToken);
    testError("1 * / 2", Parser.UnexpectedToken);
  });
});