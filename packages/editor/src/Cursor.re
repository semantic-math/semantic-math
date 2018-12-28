open EditorNode;

type cursor = {
  left: option(node),
  right: option(node),
  parent: node,
};

let nodeForPath = (path: list(int), ast) => {
  let rec _nodeForPath = (path: list(int), node) =>
    switch (path) {
    | [] => Some(node)
    | [hd, ...tl] =>
      switch (node) {
      | Box(_, _, children) =>
        try (_nodeForPath(tl, List.nth(children, hd))) {
        | Failure("nth") => None
        | Invalid_argument("List.nth") => None
        }
      | Glyph(_, _) => None
      }
    };

  /* Note: the iteration order is opposite of the normal use of the paths which usually
     has the top of the stack first.  Here we need the bottom of the stack first b/c we're
     traversing a tree. */
  _nodeForPath(List.rev(path), ast);
};

exception NoCursorForPath;

let cursorForPath = (path: list(int), ast) => {
  let rec _cursorForPath = (path: list(int), node) =>
    switch (path) {
    | [] => raise(NoCursorForPath)
    | [hd] =>
      switch (node) {
      | Box(_, _, children) =>
        let cursor = {
          left: hd > 0 ? Some(List.nth(children, hd - 1)) : None,
          right:
            hd < List.length(children) ?
              Some(List.nth(children, hd)) : None,
          parent: node,
        };
        cursor;
      | Glyph(_, _) => raise(NoCursorForPath)
      }
    | [hd, ...tl] =>
      switch (node) {
      | Box(_, _, children) => _cursorForPath(tl, List.nth(children, hd))
      | Glyph(_, _) => raise(NoCursorForPath)
      }
    };

  _cursorForPath(List.rev(path), ast);
};
