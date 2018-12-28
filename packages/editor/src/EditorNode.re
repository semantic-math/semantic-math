type node =
  | Box(int, kind, list(node))
  | Glyph(int, char)
and kind =
  | Row
  | Sup
  | Sub
  | Frac
  | Parens;

let rec toJson = node =>
  Json.Encode.(
    switch (node) {
    | Box(id, kind, children) =>
      object_([
        ("id", int(id)),
        (
          "kind",
          switch (kind) {
          | Row => string("row")
          | Sup => string("sup")
          | Sub => string("sub")
          | Frac => string("frac")
          | Parens => string("parens")
          },
        ),
        (
          "childern",
          jsonArray(Array.map(toJson, Array.of_list(children))),
        ),
      ])
    | Glyph(id, char) =>
      object_([("id", int(id)), ("char", string(String.make(1, char)))])
    }
  );

/* TODO: create function to transform the ast */
type visitor = node => option(node);

let rec transform = (visitor: visitor, node): option(node) =>
  switch (node) {
  | Box(id, kind, children) =>
    let newChildren =
      List.fold_left(
        (acc: list(node), child) =>
          switch (transform(visitor, child)) {
          | Some(node) => acc @ [node]
          | None => acc
          },
        [],
        children,
      );
    let row = Box(id, kind, newChildren);
    visitor(row);
  | _ => visitor(node)
  };

let rec traverse =
        (~parent=None, visitor: (node, option(node)) => unit, node): unit => {
  switch (node) {
  | Box(_, _, children) =>
    List.iter(
      child => traverse(~parent=Some(node), visitor, child),
      children,
    )
  | _ => ()
  };
  visitor(node, parent);
};
