/**
 * Return a transformed copy of a math AST.
 * 
 * The visitor function must return a node.  If no changes are made, it should
 * return the node it was passed.
 */
let rec transform = (visitor, node) =>
  Node.(
    switch (node.node_desc) {
    | Apply(op, children) =>
      let newChildren = List.map(transform(visitor), children);
      let apply = makeApply(op, newChildren);
      visitor({
        node_desc: apply.node_desc,
        loc: node.loc /* maintain the location of the node */
      });
    | _ => visitor(node)
    }
  );