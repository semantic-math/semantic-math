/**
 * Return a transformed copy of a math AST.
 * 
 * The visitor function must return a node.  If no changes are made, it should
 * return the node it was passed.
 */
type visitor = (~path: list(Node.node), Node.node) => option(Node.node);

let rec transform = (~path=[], visitor: visitor, node): option(Node.node) =>
  Node.(
    switch (node) {
    | (id, Apply(op, children)) =>
      let newPath = path @ [node];
      let newChildren = List.fold_left(
        (accum: list(node), child) => 
          switch (transform(~path=newPath, visitor, child)) {
          | Some(node) => accum @ [node];
          | None => accum
          }, 
        [],
        children);
      /**
       * TODO: check that List.length(newChildren) is acceptable for 
       * value of op.
       */ 
      if (List.length(newChildren) > 1) {
        let apply = makeApply(op, newChildren);
        visitor(~path=newPath, (id, apply));
      } else if (List.length(newChildren) == 1) {
        Some(List.hd(newChildren));
      } else {
        None
      }
    | _ => visitor(~path, node)
    }
  );
