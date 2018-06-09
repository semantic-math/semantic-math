open Jest;
open Expect;
open MathRules;

let testTransform = (expr, transformer, tree) =>
  test(
    expr ++ " parses as " ++ tree,
    () => {
      let tokens = Lexer.lex(expr);
      let ast = transformer.transform(MathParser.parse(tokens));
      expect(Node.toString(ast)) |> toBe(tree);
    },
  );

describe("MathRules", () => {
  describe("simplifyAddZero", () => {
    testTransform("a + 0 + b", simplifyAddZero, "[+ a b]");
    testTransform("a + 0 + b + 0", simplifyAddZero, "[+ a b]");
    testTransform("a + 0", simplifyAddZero, "a");
    testTransform("0 + 0", simplifyAddZero, "0");
    /* TODO: need to search for nodes where the transform can be applied */
    /* testTransform("a + (b + 0) + c", simplifyAddZero, "[+ a b c]"); */
  });
  describe("simplifyMulOne", () => {
    testTransform("a * 1 * b", simplifyMulOne, "[* a b]");
    testTransform("a * 1 * b * 1", simplifyMulOne, "[* a b]");
    testTransform("a * 1", simplifyMulOne, "a");
    testTransform("1 * 1", simplifyMulOne, "1");
  });
  describe("simplifyDivOne", () => {
    testTransform("a / 1", simplifyDivOne, "a");
    testTransform("a / b", simplifyDivOne, "[/ a b]");
    testTransform("1 / 1", simplifyDivOne, "1");
  });
  describe("simplifyMulZero", () => {
    testTransform("a * 0 * b", simplifyMulZero, "0");
    testTransform("a * 0", simplifyMulZero, "0");
    testTransform("0 * 0", simplifyMulZero, "0");
  });
});
