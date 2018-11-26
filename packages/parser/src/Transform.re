/**
 * Return a transformed copy of a math AST.
 * 
 * The visitor function must return a node.  If no changes are made, it should
 * return the node it was passed.
 */
let rec transform = (visitor, node) =>
  Node.(
    switch (node) {
    | (id, Apply(op, children)) =>
      let newChildren = List.map(transform(visitor), children);
      let apply = makeApply(op, newChildren);
      visitor((id, apply));
    | _ => visitor(node)
    }
  );