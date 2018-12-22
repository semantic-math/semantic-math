[%%debugger.chrome];
Js.log("NewEditor");

open Webapi.Canvas;
open Metrics;
open UniqueId;

type point = {
  mutable x: float,
  mutable y: float,
};

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

let ctx = CanvasRenderer.makeContext(1000, 600);

let cursorPath = ref([0]);

let nodeForPath = (path: list(int), ast) => {
  let rec _nodeForPath = (path: list(int), node) =>
    switch (path) {
    | [] => node
    | [hd, ...tl] =>
      switch (node) {
      | Box(_, _, children) => _nodeForPath(tl, List.nth(children, hd))
      | Glyph(_, _) => raise(Unhandled)
      }
    };

  /* Note: the iteration order is opposite of the normal use of the paths which usually
     has the top of the stack first.  Here we need the bottom of the stack first b/c we're
     traversing a tree. */
  _nodeForPath(List.rev(path), ast);
};

/* TODO: add parent */
type cursor = {
  left: option(node),
  right: option(node),
  parent: node,
};

let cursorForPath = (path: list(int), ast) => {
  let rec _cursorForPath = (path: list(int), node) =>
    switch (path) {
    | [] => raise(Unhandled)
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
      | Glyph(_, _) => raise(Unhandled)
      }
    | [hd, ...tl] =>
      switch (node) {
      | Box(_, _, children) => _cursorForPath(tl, List.nth(children, hd))
      | Glyph(_, _) => raise(Unhandled)
      }
    };

  _cursorForPath(List.rev(path), ast);
};

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

type typesetter = {typeset: node => NewLayout.node};

let makeTypesetter = (metrics: Metrics.metrics) => {
  let rec typeset = (~fontScale=1.0, ast: node): NewLayout.node =>
    switch (ast) {
    | Box(id, Row, children) => (
        Some(id),
        NewLayout.Box(
          0.,
          NewLayout.hpackNat(List.map(typeset(~fontScale), children)),
        ),
      )
    | Box(id, Sup, children) => (
        Some(id),
        NewLayout.Box(
          -30.,
          NewLayout.hpackNat(
            List.map(
              typeset(~fontScale=fontScale == 1. ? 0.8 : 0.65),
              children,
            ),
          ),
        ),
      )
    | Box(id, Parens, children) =>
      let childrenWithParens =
        [Glyph(-1, '(')] @ children @ [Glyph(-1, ')')];
      (
        Some(id),
        NewLayout.Box(
          0.,
          NewLayout.hpackNat(
            List.map(typeset(~fontScale), childrenWithParens),
          ),
        ),
      );
    | Box(id, Frac, children) =>
      let [num, den] = children;
      let children = [typeset(~fontScale, num), typeset(~fontScale, den)];
      (
        Some(id),
        NewLayout.Box(
          0.,
          {
            kind: NewLayout.VBox,
            width: NewLayout.vlistWidth(children),
            ascent: NewLayout.vlistHeight(children) /. 2.,
            descent: NewLayout.vlistHeight(children) /. 2.,
            children,
          },
        ),
      );
    | Glyph(id, char) => (
        Some(id),
        NewLayout.Glyph(char, fontScale *. 60., metrics),
      )
    | _ => (None, NewLayout.Kern(0.))
    };

  let typsetter = {typeset: typeset};

  typsetter;
};

let rec fold_left = (i: int, f: ('a, int, 'b) => 'a, accu: 'a, l: list('b)) =>
  switch (l) {
  | [] => accu
  | [h, ...t] => fold_left(i + 1, f, f(accu, i + 1, h), t)
  };

let fold_left = (f: ('a, int, 'b) => 'a, accu: 'a, l: list('b)) =>
  fold_left(0, f, accu, l);

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

exception Unhandled;
exception NotFound;

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

let drawCursor = (ctx, pen, fontSize) =>
  ctx
  |> Canvas2d.fillRect(
       ~x=pen.x,
       ~y=pen.y -. 0.85 *. fontSize,
       ~w=2.,
       ~h=fontSize,
     );

let drawChar = (ctx, pen, c, fontSize) => {
  ctx->(Canvas2d.font(string_of_float(fontSize) ++ "0px comic sans ms"));
  ctx |> Canvas2d.fillText(String.make(1, c), ~x=pen.x, ~y=pen.y);
};

let idForNode = node =>
  switch (node) {
  | Box(id, _, _) => id
  | Glyph(id, _) => id
  };

let rec renderLayout =
        (
          ctx: Webapi.Canvas.Canvas2d.t,
          layout: NewLayout.node,
          cursor: cursor,
        ) => {
  let pen = {x: 0., y: 0.};

  switch (layout) {
  | (Some(id), _) =>
    switch (cursor.right) {
    | Some(node) =>
      let cursorId = idForNode(node);
      if (cursorId == id) {
        drawCursor(ctx, pen, 60.);
      };
    | _ =>
      switch (cursor.left) {
      | Some(node) =>
        let cursorId = idForNode(node);
        if (cursorId == id) {
          let pen = {x: pen.x +. NewLayout.width(layout), y: pen.y};
          drawCursor(ctx, pen, 60.);
        };
      | _ => ()
      }
    }
  | _ => ()
  };

  switch (layout) {
  | (id, Box(shift, {kind, children})) =>
    switch (kind) {
    | NewLayout.HBox =>
      Canvas2dRe.save(ctx);
      Canvas2dRe.translate(~x=0., ~y=shift, ctx);

      let cursorIndex: int =
        switch (cursor) {
        | {left: None, right: None, parent} =>
          switch (id) {
          | Some(id) when id == idForNode(parent) =>
            switch (parent) {
            | Box(_, Parens, _) => 1
            | Box(_, Sup, _) => 0
            | Box(_, Sub, _) => 0
            | _ => (-1)
            }
          | _ => (-1)
          }
        | _ => (-1)
        };

      /* If the cursorIndex is 0, we draw it before iterating through
         the node's children.  This handles the case where children is
         an empty list. */
      if (cursorIndex == 0) {
        drawCursor(ctx, pen, 60.);
      };

      List.iteri(
        (index, child) => {
          if (index == cursorIndex) {
            drawCursor(ctx, pen, 60.);
          };
          Canvas2dRe.save(ctx);
          Canvas2dRe.translate(~x=pen.x, ~y=pen.y, ctx);
          renderLayout(ctx, child, cursor);
          Canvas2dRe.restore(ctx);
          pen.x = pen.x +. NewLayout.width(child);
        },
        children,
      );
      Canvas2dRe.restore(ctx);
    | NewLayout.VBox =>
      List.iter(
        child => {
          let height = NewLayout.vheight(child);
          Canvas2dRe.save(ctx);
          Canvas2dRe.translate(~x=pen.x, ~y=pen.y, ctx);
          renderLayout(ctx, child, cursor);
          Canvas2dRe.restore(ctx);
          pen.y = pen.y +. height;
        },
        children,
      )
    }
  | (_, Glyph(char, size, _)) => drawChar(ctx, pen, char, size)
  | _ => ()
  };
};

let rec indexOf = (x, lst, c) =>
  switch (lst) {
  | [] => raise(NotFound)
  | [hd, ...tl] => hd == x ? c : indexOf(x, tl, c + 1)
  };

let insertIntoTree = (ast, path, index, newNode) => {
  let pathNode = nodeForPath(path, ast);
  let newAst =
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
    );
  switch (newAst) {
  | Some(node) => node
  | _ => raise(Unhandled)
  };
};

Js.Promise.(
  Fetch.fetch("/packages/typesetter/metrics/comic-sans.json")
  |> then_(Fetch.Response.json)
  |> then_(json => {
       let metrics = Metrics.make(json);
       let typsetter = makeTypesetter(metrics);

       let update = () => {
         /* clear canvas */
         ctx->Canvas2d.setFillStyle(String, "#FFFFFF");
         ctx |> Canvas2d.fillRect(~x=0., ~y=0., ~w=1000., ~h=600.);

         /* set styles */
         ctx->Canvas2d.setFillStyle(String, "#000000");

         let cursor = cursorForPath(cursorPath^, ast^);
         /* let cursorNode = nodeForPath(cursorPath^, ast^);
            let id =
              switch (cursorNode) {
              | Box(id, _, _) => id
              | Glyph(id, _) => id
              };
            Js.log({j|cursorNode id = $id|j}); */
         let layout = typsetter.typeset(ast^);
         Canvas2dRe.save(ctx);
         Canvas2dRe.translate(~x=100., ~y=100., ctx);
         renderLayout(ctx, layout, cursor);
         Canvas2dRe.restore(ctx);

         /* typeset stuff */
         let pen = {x: 0., y: 300.};

         let rec render = (~fontScale=1., node, path) =>
           switch (node) {
           | Box(_, kind, children) =>
             let newFontScale =
               switch (kind) {
               | Sup
               | Sub => fontScale == 1.0 ? 0.8 : 0.65
               | _ => fontScale
               };
             /* We render the cursor for the child nodes in the parent b/c
                we don't know the index, if we could pass the index to the
                render function then we could render the cursor at the same
                time we render the glyh */
             let fontSize = newFontScale *. 60.;
             switch (kind) {
             | Sup => pen.y = pen.y -. 30.
             | Sub => pen.y = pen.y +. 30.
             | _ => ()
             };
             if (kind == Parens) {
               drawChar(ctx, pen, '(', fontSize);
               pen.x = pen.x +. metrics.getCharWidth('(', fontSize);
             };
             switch (kind) {
             | Frac =>
               let [num, den] = children;
               let {x, y} = pen;
               pen.y = pen.y -. 30.;
               render(~fontScale=newFontScale, num, [0] @ path);
               pen.x = x;
               pen.y = y;
               pen.y = pen.y +. 30.;
               render(~fontScale=newFontScale, den, [1] @ path);
               pen.y = y;
             | _ =>
               List.iteri(
                 (i, child) => {
                   if (cursorPath^ == [i] @ path) {
                     drawCursor(ctx, pen, fontSize);
                   };
                   render(~fontScale=newFontScale, child, [i] @ path);
                 },
                 children,
               )
             };
             if (cursorPath^ == [List.length(children)] @ path) {
               drawCursor(ctx, pen, fontSize);
             };
             if (kind == Parens) {
               drawChar(ctx, pen, ')', fontSize);
               pen.x = pen.x +. metrics.getCharWidth(')', fontSize);
             };
             switch (kind) {
             | Sup => pen.y = pen.y +. 30.
             | Sub => pen.y = pen.y -. 30.
             | _ => ()
             };
           | Glyph(_, c) =>
             let fontSize = fontScale *. 60.;
             /* TODO: check the previous sibling node, if it's punctuation
                then we can't drop the padding */
             switch (c) {
             | '='
             | '+'
             | '-' => pen.x = pen.x +. 15.
             | _ => ()
             };
             drawChar(ctx, pen, c, fontSize);
             pen.x = pen.x +. metrics.getCharWidth(c, fontSize);
             switch (c) {
             | '='
             | '+'
             | '-' => pen.x = pen.x +. 15.
             | _ => ()
             };
           };

         render(ast^, []);
         Js.log(ast^);
         Js.log(toJson(ast^));
       };

       update();
       open Webapi.Dom;

       Document.addKeyDownEventListener(
         event => {
           let key = KeyboardEvent.key(event);
           /* let processed = ref(false); */
           Js.log(key);
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
                   /* TODO: insert all nodes from the current box in the parent
                      at the location of the current box within the parent's children
                      list */
                   if (top == 0) {
                     cursorPath^;
                   } else {
                     let newCursorPath = [top - 1] @ parentPath;
                     let cursorNode = nodeForPath(newCursorPath, ast^);
                     let newAst =
                       transform(
                         node => node == cursorNode ? None : Some(node),
                         ast^,
                       );
                     ast :=
                       (
                         switch (newAst) {
                         | Some(node) => node
                         | _ => raise(Unhandled)
                         }
                       );
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
                   let parentNode = nodeForPath(parentPath, ast^);
                   switch (parentNode) {
                   | Box(_, _, _) =>
                     if (top == 0) {
                       /* Handle leaving a Box */
                       switch (parentPath) {
                       | [] => cursorPath^
                       | [parent, ...grandparentPath] =>
                         let grandparentNode =
                           nodeForPath(grandparentPath, ast^);
                         switch (grandparentNode) {
                         /* navigate out of numerator */
                         | Box(_, Frac, _) when parent == 0 => grandparentPath
                         /* navigate from denominator to numerator */
                         | Box(_, Frac, _) when parent == 1 =>
                           let prevParentNode =
                             nodeForPath([0] @ grandparentPath, ast^);
                           switch (prevParentNode) {
                           | Box(_, _, children) =>
                             [List.length(children), 0] @ grandparentPath
                           | Glyph(_, _) => raise(Unhandled)
                           };
                         /* navigate out of child to parent */
                         | _ => parentPath
                         };
                       };
                     } else {
                       let prevNode =
                         nodeForPath([top - 1] @ parentPath, ast^);
                       switch (prevNode) {
                       | Box(_, kind, children) =>
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
                           | Glyph(_, _) => raise(Unhandled)
                           };
                         | _ => [List.length(children), top - 1] @ parentPath
                         }
                       | Glyph(_, _) => [top - 1] @ parentPath
                       };
                     }
                   | Glyph(_, _) => raise(Unhandled) /* Glyphs don't have children */
                   };
                 }
               )
           | "ArrowRight" =>
             cursorPath :=
               (
                 switch (cursorPath^) {
                 | [] => []
                 | [top, ...parentPath] =>
                   let parentNode = nodeForPath(parentPath, ast^);
                   switch (parentNode) {
                   | Box(_, _, children) =>
                     if (top == List.length(children)) {
                       switch (parentPath) {
                       | [] => cursorPath^
                       | [parent, ...grandparentPath] =>
                         let grandparentNode =
                           nodeForPath(grandparentPath, ast^);
                         switch (grandparentNode) {
                         /* Navigate from numerator to denominator */
                         | Box(_, Frac, _) when parent == 0 =>
                           [0, 1] @ grandparentPath
                         /* Navigate out of denominator */
                         | Box(_, Frac, _) when parent == 1 =>
                           let [gp, ...rest] = grandparentPath;
                           [gp + 1, ...rest];
                         /* Navigate out of child to parent */
                         | _ => [parent + 1] @ grandparentPath
                         };
                       };
                     } else {
                       let nextNode = nodeForPath(cursorPath^, ast^);
                       switch (nextNode) {
                       /* Handle entering a Box */
                       | Box(_, kind, _) =>
                         switch (kind) {
                         /* If it's a fraction, navigate into first child of the numerator */
                         | Frac => [0, 0] @ cursorPath^
                         | _ => [0] @ cursorPath^
                         }
                       | Glyph(_, _) => [top + 1] @ parentPath
                       };
                     }
                   | Glyph(_, _) => raise(Unhandled) /* Glyphs don't have children */
                   };
                 }
               )
           | "^" =>
             let sup = Box(genId(), Sup, [Glyph(genId(), '2')]);
             cursorPath :=
               (
                 switch (cursorPath^) {
                 | [] => []
                 | [top, ...parentPath] =>
                   ast := insertIntoTree(ast^, parentPath, top, sup);
                   [1, top] @ parentPath;
                 }
               );
           | "_" =>
             let sub = Box(genId(), Sub, [Glyph(genId(), '2')]);
             cursorPath :=
               (
                 switch (cursorPath^) {
                 | [] => []
                 | [top, ...parentPath] =>
                   ast := insertIntoTree(ast^, parentPath, top, sub);
                   [1, top] @ parentPath;
                 }
               );
           | "/" =>
             let num = Box(genId(), Row, [Glyph(genId(), '1')]);
             let den =
               Box(
                 genId(),
                 Row,
                 [
                   Glyph(genId(), 'x'),
                   Glyph(genId(), '+'),
                   Glyph(genId(), '1'),
                 ],
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
                     insertIntoTree(
                       ast^,
                       parentPath,
                       top,
                       Glyph(genId(), key.[0]),
                     );
                   [top + 1] @ parentPath;
                 }
               )
           };
           update();
         },
         document,
       );

       resolve();
     })
);