/* TODO(kevinb): store the type signatures of operators for semantic analysis */
type node_desc =
  | Apply(operator, list(node))
  | Identifier(string)
  | Number(string)
and operator =
  | Add
  | Sub
  | Mul([ | `Explicit | `Implicit])
  | Div
  | Exp
  | Neg
  | Pos
  | Eq
  | Gt
  | Lt
  | Func(node)
  | /**
   * These aren't actually operators but they do appear temporarily in the
   * operandStack so we include them here to handle those situations.
   */
    LeftParen
  | RightParen
  | Comma
and node = {
  node_desc,
  loc: Lexer.location,
};

exception NoOperands;

let makeApply = (op: operator, children: list(node)) => {
  let len = List.length(children);
  switch (List.hd(children), List.nth(children, len - 1)) {
  | (first, last) => {
      node_desc: Apply(op, children),
      loc: {
        Lexer.start: first.loc.Lexer.start,
        Lexer.end_: last.loc.Lexer.end_,
      },
    }
  | exception _ => raise(NoOperands)
  };
};

let makeNumber = (token: Lexer.token) => {
  /* TODO(kevinb): verify that it's a number token */
  node_desc: Number(token.Lexer.value),
  loc: token.Lexer.loc,
};

let makeIdentifier = (token: Lexer.token) => {
  /* TODO(kevinb): verify that it's an identifier token */
  node_desc: Identifier(token.Lexer.value),
  loc: token.Lexer.loc,
};

let makeNewIdentifier = (id: string, start: int, end_: int) => {
  node_desc: Identifier(id),
  loc: {
    Lexer.start,
    Lexer.end_,
  },
};

let rec nodeToString = node =>
  switch (node.node_desc) {
  | Apply(op, children) =>
    "["
    ++ Js.Array.joinWith(
         " ",
         Array.of_list([
           opToString(op),
           ...List.map(nodeToString, children),
         ]),
       )
    ++ "]"
  | Identifier(name) => name
  | Number(value) => value
  }
and opToString = (op: operator) =>
  switch (op) {
  | Add => "+"
  | Sub => "-"
  | Mul(_) => "*"
  | Div => "/"
  | Neg => "neg"
  | Pos => "pos"
  | Exp => "^"
  | Eq => "="
  | LeftParen => "("
  | RightParen => ")"
  | Gt => ">"
  | Lt => "<"
  | Func(node) => nodeToString(node)
  | Comma => ","
  };

let precedence = op =>
  switch (op) {
  | Comma => (-1)
  | Eq
  | Lt
  | Gt => 0
  | LeftParen
  | RightParen => 1
  | Add
  | Sub => 2
  | Mul(`Explicit) => 3
  | Mul(`Implicit)
  | Div => 4
  | Neg
  | Pos => 5
  | Exp => 6
  | Func(_) => 7
  };

let wellKnownIdentifiers = [
  /* greek letters */
  "alpha",
  "beta",
  "gamma",
  "delta",
  "epsilon",
  "pi",
  "tau",
  "theta",
  /* trig functions */
  /* TODO(kevinb): store the type signature of functions for semantic analysis */
  "sin",
  "cos",
  "tan",
  "sec",
  "csc",
  "cot",
  "asin",
  "acos",
  "atan",
  "asec",
  "acsc",
  "acot",
];

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

let last = arr =>
  switch (Array.length(arr)) {
  | 0 => None
  | n => Some(arr[n - 1])
  };

module Stack = Belt.MutableStack;

let rec traverse = (visitor, node) =>
  switch (node.node_desc) {
  | Apply(_, children) =>
    List.iter(traverse(visitor), children);
    visitor(node);
  | _ => visitor(node)
  };

let rec transform = (visitor: node => node, node) : node =>
  switch (node.node_desc) {
  | Apply(op, children) =>
    let newChildren = List.map(transform(visitor), children);
    let apply = makeApply(op, newChildren);
    visitor({
      node_desc: apply.node_desc,
      loc: node.loc /* maintain the location of the node */
    });
  | _ => visitor(node)
  };

/* return a node */
let parse = (tokens: array(Lexer.token), str: string) => {
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
        switch (op) {
        | LeftParen =>
          if (all) {
            raise(Unmatched_left_paren);
          } else {
            switch (Stack.top(operandStack)) {
            | Some(top) =>
              /* Update the top operand to include the parens in the loc */
              replaceTop(
                operandStack,
                {
                  node_desc: top.node_desc,
                  loc: {
                    Lexer.start: otherLoc.Lexer.start,
                    Lexer.end_: loc.Lexer.end_,
                  },
                },
              )
            | _ => ()
            };
            break := true;
          };
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
      parseOp(~collate=collate, op, loc);
    | _ => Stack.push(operatorStack, (op, 2, loc))
    };
  /* Process each token. */
  tokens
  |> Array.iteri((i, token) => {
       let prevToken = i > 0 ? Some(tokens[i - 1]) : None;
       let nextToken =
         i < Array.length(tokens) - 1 ? Some(tokens[i + 1]) : None;
       switch (token.Lexer.t) {
       | Lexer.IDENTIFIER(name) =>
         switch (String.length(name)) {
         | 0 => raise(Empty_identifier)
         | 1 => Stack.push(operandStack, makeIdentifier(token))
         | _ when ! List.mem(name, wellKnownIdentifiers) =>
           /* turn multi-character identifiers into multiplication */
           let letters = Js.String.split("", name);
           Stack.push(
             operandStack,
             makeNewIdentifier(
               letters[0],
               token.Lexer.loc.Lexer.start,
               token.Lexer.loc.Lexer.start + 1,
             ),
           );
           for (j in 1 to Array.length(letters) - 1) {
             parseOp(~collate=true, Mul(`Implicit), token.loc);
             Stack.push(
               operandStack,
               makeNewIdentifier(
                 letters[j],
                 token.Lexer.loc.Lexer.start + j,
                 token.Lexer.loc.Lexer.start + j + 1,
               ),
             );
           };
         | _ => Stack.push(operandStack, makeIdentifier(token))
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
       | op => raise(Unknown_operator(Lexer.tokenTypeToString(op)))
       };
     });
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
    | _ => raise(Missing_operator)
    };
  /* Js.log(nodeToString(result)); */
  /**
   * TOOD(kevinb): add transform here to convert implicit multiplication to
   * a Func call
   */
  transform(
    node =>
      switch (node.node_desc) {
      | Apply(Mul(`Implicit), children) =>
        if (List.length(children) == 2) {
          let first = List.nth(children, 0);
          let firstSubstr =
            Js.String.slice(
              ~from=first.loc.Lexer.start,
              ~to_=first.loc.Lexer.end_,
              str,
            );
          let last = List.nth(children, 1);
          let lastSubstr =
            Js.String.slice(
              ~from=last.loc.Lexer.start,
              ~to_=last.loc.Lexer.end_,
              str,
            );
          if (Js.String.startsWith("(", lastSubstr)
              && ! Js.String.endsWith(")", firstSubstr)) {
            switch (last.node_desc) {
            | Apply(Comma, args) => {
                node_desc: Apply(Func(first), args),
                loc: {
                  Lexer.start: first.loc.Lexer.start,
                  Lexer.end_: last.loc.Lexer.end_,
                },
              }
            | _ => {
                node_desc: Apply(Func(first), [last]),
                loc: {
                  Lexer.start: first.loc.Lexer.start,
                  Lexer.end_: last.loc.Lexer.end_,
                },
              }
            };
          } else {
            node;
          };
        } else {
          node;
        }
      | _ => node
      },
    result,
  );
};