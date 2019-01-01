[%%debugger.chrome];
Js.log("NewEditor");

open Webapi.Canvas;
open UniqueId;
open EditorNode;
open EditorTypsetter;
open EditorRenderer;
open Cursor;
open Tree;

let cursorPath = ref([0]);

let ast =
  ref(
    Box(
      genId(),
      Row,
      [
        Glyph(genId(), '2'),
        Glyph(genId(), 'x'),
        Glyph(genId(), '+'),
        Glyph(genId(), '5'),
        Glyph(genId(), '='),
        Glyph(genId(), '1'),
        Glyph(genId(), '0'),
      ],
    ),
  );

exception Unhandled;

let rec remove_at = (n: int, l: list('a)) =>
  switch (l) {
  | [] => []
  | [h, ...t] => n == 0 ? t : [h, ...remove_at(n - 1, t)]
  };

let rec insert_at = (n: int, x: 'a, l: list('a)) =>
  switch (l) {
  | [] => n == 0 ? [x] : []
  | [h, ...t] => n == 0 ? [x, h, ...t] : [h, ...insert_at(n - 1, x, t)]
  };

let rec insert_many_at = (n: int, xs: list('a), l: list('a)) =>
  switch (l) {
  | [] => n == 0 ? xs : []
  | [h, ...t] =>
    n == 0 ? xs @ [h, ...t] : [h, ...insert_many_at(n - 1, xs, t)]
  };

let insertIntoTree = (ast, path, index, newNode) => {
  let newAst =
    switch (nodeForPath(path, ast)) {
    | Some(pathNode) =>
      transform(
        node =>
          node == pathNode ?
            switch (pathNode) {
            | Box(id, kind, children) =>
              Some(Box(id, kind, insert_at(index, newNode, children)))
            | Glyph(_, _) => raise(Unhandled)
            } :
            Some(node),
        ast,
      )
    | None => raise(Unhandled)
    };
  switch (newAst) {
  | Some(node) => node
  | _ => raise(Unhandled)
  };
};

let insertManyIntoTree = (ast, path, index, newNodes) => {
  let newAst =
    switch (nodeForPath(path, ast)) {
    | Some(pathNode) =>
      transform(
        node =>
          node == pathNode ?
            switch (pathNode) {
            | Box(id, kind, children) =>
              Some(Box(id, kind, insert_many_at(index, newNodes, children)))
            | Glyph(_, _) => raise(Unhandled)
            } :
            Some(node),
        ast,
      )
    | None => raise(Unhandled)
    };
  switch (newAst) {
  | Some(node) => node
  | _ => raise(Unhandled)
  };
};

let removeNode = (nodeToRemove, ast) => {
  let newAst =
    transform(
      node => node == nodeToRemove ? None : Some(node),
      ast,
    );
  switch (newAst) {
  | Some(node) => node
  | _ => raise(Unhandled)
  }
};

exception NoNodeForPath;

type cursor_path = list(int);

let processEvent =
    (key: string, cursorPath: ref(cursor_path), ast: ref(node)) =>
  switch (key) {
  | "Meta"
  | "Shift"
  | "Alt"
  | "Control" => ignore()
  | "Backspace" =>
    cursorPath :=
      (
        switch (cursorPath^) {
        | [] => []
        | [top, ...parentPath] =>
          if (top == 0) {
            let cursor = cursorForPath(cursorPath^, ast^);
            let parentNode =
              switch (nodeForPath(parentPath, ast^)) {
              | Some(node) => node
              | None => raise(NoNodeForPath)
              };
            if (cursor.left == None && cursor.right == None) {
              /* The container is empty so let's delete it. */
              ast := removeNode(parentNode, ast^);
              parentPath;
            } else {
              /* If there are some child nodes, move them into the parent. */
              let Box(_, _, children) = parentNode;
              let [parent, ...grandparentPath] = parentPath;
              ast := removeNode(parentNode, ast^);
              ast := insertManyIntoTree(ast^, grandparentPath, parent, children);
              parentPath;
            };
          } else {
            let newCursorPath = [top - 1] @ parentPath;
            let cursorNode =
              switch (nodeForPath(newCursorPath, ast^)) {
              | Some(node) => node
              | None => raise(NoNodeForPath)
              };
            ast := removeNode(cursorNode, ast^);
            newCursorPath;
          }
        }
      )
  | "ArrowLeft" =>
    cursorPath :=
      (
        switch (cursorPath^) {
        | [] => []
        | [top, ...parentPath] =>
          switch (nodeForPath(parentPath, ast^)) {
          | Some(Box(_, _, _)) =>
            if (top == 0) {
              /* Handle leaving a Box */
              switch (parentPath) {
              | [] => cursorPath^
              | [parent, ...grandparentPath] =>
                switch (nodeForPath(grandparentPath, ast^)) {
                /* navigate out of numerator */
                | Some(Box(_, Frac, _)) when parent == 0 => grandparentPath

                /* navigate from denominator to numerator */
                | Some(Box(_, Frac, _)) when parent == 1 =>
                  switch (nodeForPath([0] @ grandparentPath, ast^)) {
                  | Some(Box(_, _, children)) =>
                    [List.length(children), 0] @ grandparentPath
                  | _ => raise(NoNodeForPath)
                  }

                /* navigate out of child to parent */
                | _ => parentPath
                }
              };
            } else {
              switch (nodeForPath([top - 1] @ parentPath, ast^)) {
              | Some(Box(_, kind, children)) =>
                /* Handle entering a Box */
                switch (kind) {
                /* If it's a fraction, navigate to the last child of the denominator */
                | Frac =>
                  let lastChild =
                    List.nth(children, List.length(children) - 1);
                  switch (lastChild) {
                  | Box(_, _, grandchildren) =>
                    [
                      List.length(grandchildren),
                      List.length(children) - 1,
                      top - 1,
                    ]
                    @ parentPath
                  | Glyph(_, _) => raise(NoNodeForPath)
                  };
                | _ => [List.length(children), top - 1] @ parentPath
                }
              | Some(Glyph(_, _)) => [top - 1] @ parentPath
              | _ => raise(NoNodeForPath)
              };
            }
          | _ => raise(NoNodeForPath) /* Glyphs don't have children */
          }
        }
      )
  | "ArrowRight" =>
    cursorPath :=
      (
        switch (cursorPath^) {
        | [] => []
        | [top, ...parentPath] =>
          switch (nodeForPath(parentPath, ast^)) {
          | Some(Box(_, _, children)) =>
            if (top == List.length(children)) {
              switch (parentPath) {
              | [] => cursorPath^
              | [parent, ...grandparentPath] =>
                switch (nodeForPath(grandparentPath, ast^)) {
                /* Navigate from numerator to denominator */
                | Some(Box(_, Frac, _)) when parent == 0 =>
                  [0, 1] @ grandparentPath

                /* Navigate out of denominator */
                | Some(Box(_, Frac, _)) when parent == 1 =>
                  switch (grandparentPath) {
                  | [gp, ...rest] => [gp + 1, ...rest]
                  | _ => raise(Unhandled)
                  }

                /* Navigate out of child to parent */
                | _ => [parent + 1] @ grandparentPath
                }
              };
            } else {
              switch (nodeForPath(cursorPath^, ast^)) {
              /* Handle entering a Box */
              | Some(Box(_, kind, _)) =>
                switch (kind) {
                /* If it's a fraction, navigate into first child of the numerator */
                | Frac => [0, 0] @ cursorPath^
                | _ => [0] @ cursorPath^
                }
              | Some(Glyph(_, _)) => [top + 1] @ parentPath
              | _ => raise(Unhandled)
              };
            }
          | _ => raise(Unhandled) /* Glyphs don't have children */
          }
        }
      )
  | "^" =>
    switch (nodeForPath(cursorPath^, ast^)) {
    | Some(Box(_, Sup, _)) => cursorPath := [0] @ cursorPath^
    | _ =>
      let sup = Box(genId(), Sup, []);
      cursorPath :=
        (
          switch (cursorPath^) {
          | [] => []
          | [top, ...parentPath] =>
            ast := insertIntoTree(ast^, parentPath, top, sup);
            [0, top] @ parentPath;
          }
        );
    }
  | "_" =>
    switch (nodeForPath(cursorPath^, ast^)) {
    | Some(Box(_, Sub, _)) => cursorPath := [0] @ cursorPath^
    | _ =>
      let sub = Box(genId(), Sub, []);
      cursorPath :=
        (
          switch (cursorPath^) {
          | [] => []
          | [top, ...parentPath] =>
            ast := insertIntoTree(ast^, parentPath, top, sub);
            [0, top] @ parentPath;
          }
        );
    }
  | "/" =>
    let num = Box(genId(), Row, [Glyph(genId(), '1')]);
    let den =
      Box(
        genId(),
        Row,
        [Glyph(genId(), 'x'), Glyph(genId(), '+'), Glyph(genId(), '1')],
      );
    let frac = Box(genId(), Frac, [num, den]);
    cursorPath :=
      (
        switch (cursorPath^) {
        | [] => []
        | [top, ...parentPath] =>
          ast := insertIntoTree(ast^, parentPath, top, frac);
          [top + 1] @ parentPath;
        }
      );
  | "(" =>
    let parens = Box(genId(), Parens, []);
    cursorPath :=
      (
        switch (cursorPath^) {
        | [] => []
        | [top, ...parentPath] =>
          ast := insertIntoTree(ast^, parentPath, top, parens);
          [0, top] @ parentPath;
        }
      );
  | _ =>
    cursorPath :=
      (
        switch (cursorPath^) {
        | [] => []
        | [top, ...parentPath] =>
          ast :=
            insertIntoTree(ast^, parentPath, top, Glyph(genId(), key.[0]));
          [top + 1] @ parentPath;
        }
      )
  };

let ctx = CanvasRenderer.makeContext(1600, 600);

Js.Promise.(
  Fetch.fetch("/packages/typesetter/metrics/comic-sans.json")
  |> then_(Fetch.Response.json)
  |> then_(json => {
       let metrics = Metrics.make(json);

       let treeTypesetter = TreeTypesetter.makeTypesetter(metrics);
       let treeLayout = treeTypesetter.typeset(Tree.tree);

       let update = () => {
         /* clear canvas */
         ctx->Canvas2d.setFillStyle(String, "#FFFFFF");
         ctx |> Canvas2d.fillRect(~x=0., ~y=0., ~w=1600., ~h=600.);

         /* set styles */
         ctx->Canvas2d.setFillStyle(String, "#000000");

         Canvas2dRe.save(ctx);
         Canvas2dRe.translate(~x=100., ~y=300., ctx);
         renderLayout(ctx, treeLayout, Tree.cursor);
         Canvas2dRe.restore(ctx);

         Js.log(ast^);
         Js.log(toJson(ast^));
       };

       update();
       open Webapi.Dom;

       Document.addKeyDownEventListener(
         event => {
           let key = KeyboardEvent.key(event);
           processEvent(key, cursorPath, ast);
           update();
         },
         document,
       );

       resolve();
     })
);