/**
 * Traverse a math AST and call visitor with each node.
 */
let rec traverse = (visitor, node) => {
  let (_, typ) = node;
  switch (typ) {
  | Node.Apply(_, children) =>
    List.iter(traverse(visitor), children);
    visitor(node);
  | _ => visitor(node)
  };
}