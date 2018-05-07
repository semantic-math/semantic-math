/**
 * Traverse a math AST and call visitor with each node.
 */
let rec traverse = (visitor, node) =>
  Node.(
    switch (node.node_desc) {
    | Node.Apply(_, children) =>
      List.iter(traverse(visitor), children);
      visitor(node);
    | _ => visitor(node)
    }
  );