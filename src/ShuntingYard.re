exception Missing_operator;

exception Missing_operand;

exception Unmatched_left_paren;

exception Unmatched_right_paren;

exception Unknown_error;

exception Unknown_operator(string);

exception Empty_identifier;

let replaceTop = (stack, value) => {
  Belt.MutableStack.pop(stack) |> ignore;
  Belt.MutableStack.push(stack, value);
};

let substrForLoc = (str, loc) =>
  Js.String.slice(~from=loc.Lexer.start, ~to_=loc.Lexer.end_, str);

module Stack = Belt.MutableStack;

/* return a node */
let parse = (tokens, str) => {
  open Node;
  let operatorStack: Stack.t((operator, int, Lexer.location)) = Stack.make();
  let operandStack: Stack.t(node) = Stack.make();
  let rec popOperands = arity : list(node) =>
    if (arity == 0) {
      [];
    } else {
      switch (Belt.MutableStack.pop(operandStack)) {
      | Some(value) => [value, ...popOperands(arity - 1)]
      | None => raise(Missing_operand)
      };
    };
  /**
   * Pop all operations or until the first left parenthesis is encountered.String
   *
   * As operations are popped from the operationStack, Apply(op, array(node))
   * nodes are created based on the arity of op and pushed on to operandStack.
   */
  let popOperations = (~all: bool, ~loc: Lexer.location) => {
    let break = ref(false);
    while (! break^) {
      switch (Stack.pop(operatorStack)) {
      | Some((op, arity, otherLoc)) =>
        switch (op, all) {
        | (LeftParen, true) => raise(Unmatched_left_paren)
        | (LeftParen, false) =>
          switch (Stack.top(operandStack)) {
          | Some(top) =>
            /* Update the top operand to include the parens in the loc */
            replaceTop(
              operandStack,
              updateLoc(top, otherLoc.Lexer.start, loc.Lexer.end_),
            )
          | _ => ()
          };
          break := true;
        | _ =>
          let children = List.rev(popOperands(arity));
          Stack.push(operandStack, makeApply(op, children));
        }
      | None => all ? break := true : raise(Unmatched_right_paren)
      };
    };
  };
  /**
   * op: Lexer.token - operator to parse
   * ~collate: boolean - combine multiple operators into n-ary operator
   */
  let rec parseOp = (~collate=false, op, loc) =>
    switch (Stack.top(operatorStack)) {
    | Some((topOp, arity, loc)) when op == topOp && collate =>
      replaceTop(operatorStack, (op, arity + 1, loc))
    | Some((topOp, arity, loc))
        when topOp != LeftParen && precedence(op) < precedence(topOp) =>
      Stack.pop(operatorStack) |> ignore;
      let children = List.rev(popOperands(arity));
      Stack.push(operandStack, makeApply(topOp, children));
      /* case where the revealed operator matches the new operator */
      /* we recurse he b/c there may be multiple operators increasing precedence
         that can be reveal */
      parseOp(~collate, op, loc);
    | _ => Stack.push(operatorStack, (op, 2, loc))
    };
  let splitIdentifier = (token: Lexer.token, name) => {
    let letters = Js.String.split("", name);
    for (j in 0 to Array.length(letters) - 1) {
      if (j > 0) {
        /* TODO: update location to be in between letters */
        parseOp(
          ~collate=true,
          Mul(`Implicit),
          token.loc,
        );
      };
      Stack.push(
        operandStack,
        makeNewIdentifier(
          letters[j],
          token.Lexer.loc.Lexer.start + j,
          token.Lexer.loc.Lexer.start + j + 1,
        ),
      );
    };
  };
  /* Process a single token */
  let processToken = (token, prevToken, nextToken) =>
    switch (token.Lexer.t) {
    | Lexer.IDENTIFIER(name) =>
      switch (String.length(name)) {
      | 0 => raise(Empty_identifier)
      | 1 => Stack.push(operandStack, makeIdentifier(token))
      | _ when List.mem(name, Data.wellKnownIdentifiers) =>
        Stack.push(operandStack, makeIdentifier(token))
      | _ => splitIdentifier(token, name)
      }
    | Lexer.NUMBER(_) =>
      Stack.push(operandStack, makeNumber(token));
      switch (nextToken) {
      | Some({Lexer.t: Lexer.IDENTIFIER(_)}) =>
        parseOp(~collate=true, Mul(`Implicit), token.loc)
      | _ => ()
      };
    | Lexer.RIGHT_PAREN => popOperations(~all=false, ~loc=token.loc)
    | Lexer.LEFT_PAREN =>
      switch (prevToken) {
      | Some({Lexer.t: Lexer.IDENTIFIER(_)})
      | Some({Lexer.t: Lexer.NUMBER(_)})
      | Some({Lexer.t: Lexer.RIGHT_PAREN}) =>
        parseOp(~collate=true, Mul(`Implicit), token.loc)
      | _ => ()
      };
      /* TODO(kevinb): post-process implicit multiplication to detect Funcs */
      Stack.push(operatorStack, (LeftParen, 0, token.loc));
    | Lexer.PLUS => parseOp(~collate=true, Add, token.loc)
    | Lexer.STAR => parseOp(~collate=true, Mul(`Explicit), token.loc)
    | Lexer.MINUS =>
      /**
       * If we see a minus after a number, identifier, or right parenthesis
       * we know that it's actually subtraction.  We treat subtraction as
       * adding the negative.
       */
      (
        switch (prevToken) {
        | Some({Lexer.t: Lexer.NUMBER(_)})
        | Some({Lexer.t: Lexer.IDENTIFIER(_)})
        | Some({Lexer.t: Lexer.RIGHT_PAREN}) =>
          parseOp(~collate=true, Add, token.loc)
        | _ => ()
        }
      );
      Stack.push(operatorStack, (Neg, 1, token.loc));
    | Lexer.CARET => parseOp(Exp, token.loc)
    | Lexer.EQUAL => parseOp(~collate=true, Eq, token.loc)
    | Lexer.COMMA => parseOp(~collate=true, Comma, token.loc)
    | Lexer.SLASH => parseOp(Div, token.loc)
    | Lexer.LESS_THAN => parseOp(~collate=true, Lt, token.loc)
    | Lexer.LESS_THAN_OR_EQUAL => parseOp(~collate=true, Lte, token.loc)
    | Lexer.GREATER_THAN => parseOp(~collate=true, Gt, token.loc)
    | Lexer.GREATER_THAN_OR_EQUAL => parseOp(~collate=true, Gte, token.loc)
    | _ => raise(Unknown_error)
    };
  /* Process each token. */
  Array.iteri(
    (i, token) => {
      let prevToken = i > 0 ? Some(tokens[i - 1]) : None;
      let nextToken =
        i < Array.length(tokens) - 1 ? Some(tokens[i + 1]) : None;
      processToken(token, prevToken, nextToken);
    },
    tokens,
  );
  /* Clean up any operators that are still on the operator stack. */
  popOperations(~all=true, ~loc={Lexer.start: (-1), Lexer.end_: (-1)});
  /* Check if we have a single value left and return that value in that case. */
  let result =
    switch (Stack.size(operandStack)) {
    | 0 => raise(Missing_operand)
    | 1 =>
      switch (Stack.pop(operandStack)) {
      | Some(value) => value
      | None => raise(Unknown_error) /* should never happen b/c we checked the size already */
      }
    | _ =>
      Stack.forEach(operatorStack, Js.log);
      raise(Missing_operator);
    };
  /* Js.log(nodeToString(result)); */
  /**
   * TOOD(kevinb): add transform here to convert implicit multiplication to
   * a Func call
   */
  Transform.transform(
    node =>
      switch (node.node_desc) {
      | Apply(Mul(`Implicit), children) =>
        if (List.length(children) == 2) {
          let first = List.nth(children, 0);
          let firstSubstr = substrForLoc(str, first.loc);
          let last = List.nth(children, 1);
          let lastSubstr = substrForLoc(str, last.loc);
          /**
           * TOOD(kevinb): eventually we'll want to handle expressions like
           * (f + g)(x), but this depends on semanitc knowledge of what f and g
           * are.  For instance (x)(y) may parse as multiplication, but (f+g)(x)
           * may parse as a function
           */
          (
            if (Js.String.startsWith("(", lastSubstr)
                && ! Js.String.endsWith(")", firstSubstr)) {
              let args =
                switch (last.node_desc) {
                | Apply(Comma, args) => args
                | _ => [last]
                };
              {
                node_desc: Apply(Func(first), args),
                loc: {
                  Lexer.start: first.loc.Lexer.start,
                  Lexer.end_: last.loc.Lexer.end_,
                },
              };
            } else {
              node;
            }
          );
        } else {
          node;
        }
      | _ => node
      },
    result,
  );
};