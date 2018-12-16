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

let rec nodeForPath = (path: list(int), node) =>
  switch (path) {
  | [] => node
  | [hd, ...tl] =>
    switch (node) {
    | Box(_, _, children) => nodeForPath(tl, List.nth(children, hd))
    | Glyph(_, _) => raise(Unhandled)
    }
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
  ctx |> Canvas2d.fillRect(~x=pen.x, ~y=pen.y -. 0.85 *. fontSize, ~w=2., ~h=fontSize);

let drawChar = (ctx, pen, c, fontSize) => {
  ctx->(Canvas2d.font(string_of_float(fontSize) ++ "0px comic sans ms"));
  ctx |> Canvas2d.fillText(String.make(1, c), ~x=pen.x, ~y=pen.y);
};

let rec indexOf = (x, lst, c) =>
  switch (lst) {
  | [] => raise(NotFound)
  | [hd, ...tl] => hd == x ? c : indexOf(x, tl, c + 1)
  };

let insertIntoTree = (ast, parentNode, index, newNode) => {
  let newAst =
    transform(
      node =>
        node == parentNode ?
          switch (parentNode) {
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

       Js.log(json);

       let update = () => {
         /* clear canvas */
         ctx->Canvas2d.setFillStyle(String, "#FFFFFF");
         ctx |> Canvas2d.fillRect(~x=0., ~y=0., ~w=1000., ~h=600.);

         /* set styles */
         ctx->Canvas2d.setFillStyle(String, "#000000");

         Js.log(cursorPath);

         /* typeset stuff */
         let pen = {x: 0., y: 300.};

         let rec render = (~fontScale=1., node, path) => {
           switch (node) {
           | Box(_, kind, children) =>
             let newFontScale = switch(kind) {
             | Sup | Sub => fontScale == 1.0 ? 0.8 : 0.65
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
             List.iteri(
               (i, child) => {
                 if (cursorPath^ == path @ [i]) {
                   drawCursor(ctx, pen, fontSize);
                 };
                 render(~fontScale=newFontScale, child, path @ [i]);
               },
               children,
             );
             if (cursorPath^ == path @ [List.length(children)]) {
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
                 switch (List.rev(cursorPath^)) {
                 | [] => []
                 | [last, ...revParentPath] =>
                   /* TODO: insert all nodes from the current box in the parent
                      at the location of the current box within the parent's children
                      list */
                   if (last == 0) {
                     cursorPath^;
                   } else {
                     let newCursorPath =
                       List.rev_append(revParentPath, [last - 1]);
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
                 switch (List.rev(cursorPath^)) {
                 | [] => []
                 | [last, ...revParentPath] =>
                   let parentPath = List.rev(revParentPath);
                   let parentNode = nodeForPath(parentPath, ast^);
                   switch (parentNode) {
                   | Box(_, _, _) =>
                     if (last == 0) {
                       /* Handle leaving a Box */
                       switch (List.rev(parentPath)) {
                       | [] => cursorPath^
                       | _ =>
                         /* List.rev_append(revGrandparentPath, [last - 1]) */
                         parentPath
                       };
                     } else {
                       let prevNode =
                         nodeForPath(
                           List.rev_append(revParentPath, [last - 1]),
                           ast^,
                         );
                       switch (prevNode) {
                       | Box(_, _, children) =>
                         /* Handle entering a Box */
                         List.rev_append(revParentPath, [last - 1])
                         @ [List.length(children)]
                       | Glyph(_, _) =>
                         List.rev_append(revParentPath, [last - 1])
                       };
                     }
                   | Glyph(_, _) => raise(Unhandled) /* Glyphs don't have children */
                   };
                 }
               )
           | "ArrowRight" =>
             cursorPath :=
               (
                 switch (List.rev(cursorPath^)) {
                 | [] => []
                 | [last, ...revParentPath] =>
                   let parentPath = List.rev(revParentPath);
                   let parentNode = nodeForPath(parentPath, ast^);
                   switch (parentNode) {
                   | Box(_, _, children) =>
                     if (last == List.length(children)) {
                       switch (List.rev(parentPath)) {
                       | [] => cursorPath^
                       | [last, ...revGrandparentPath] =>
                         List.rev_append(revGrandparentPath, [last + 1])
                       };
                     } else {
                       let nextNode = nodeForPath(cursorPath^, ast^);
                       switch (nextNode) {
                       | Box(_, _, _) => cursorPath^ @ [0]
                       | Glyph(_, _) =>
                         List.rev_append(revParentPath, [last + 1])
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
                 switch (List.rev(cursorPath^)) {
                 | [] => []
                 | [last, ...revParentPath] =>
                   let parentNode =
                     nodeForPath(List.rev(revParentPath), ast^);
                   ast := insertIntoTree(ast^, parentNode, last, sup);
                   List.rev_append(revParentPath, [last + 1]);
                 }
               );
           | "_" =>
             let sub = Box(genId(), Sub, [Glyph(genId(), '2')]);
             cursorPath :=
               (
                 switch (List.rev(cursorPath^)) {
                 | [] => []
                 | [last, ...revParentPath] =>
                   let parentNode =
                     nodeForPath(List.rev(revParentPath), ast^);
                   ast := insertIntoTree(ast^, parentNode, last, sub);
                   List.rev_append(revParentPath, [last + 1]);
                 }
               );
           | "(" =>
             let parens = Box(genId(), Parens, []);
             cursorPath :=
               (
                 switch (List.rev(cursorPath^)) {
                 | [] => []
                 | [last, ...revParentPath] =>
                   let parentNode =
                     nodeForPath(List.rev(revParentPath), ast^);
                   ast := insertIntoTree(ast^, parentNode, last, parens);
                   List.rev_append(revParentPath, [last + 1]);
                 }
               );
           | _ =>
             cursorPath :=
               (
                 switch (List.rev(cursorPath^)) {
                 | [] => []
                 | [last, ...revParentPath] =>
                   let parentNode =
                     nodeForPath(List.rev(revParentPath), ast^);
                   ast :=
                     insertIntoTree(
                       ast^,
                       parentNode,
                       last,
                       Glyph(genId(), key.[0]),
                     );
                   List.rev_append(revParentPath, [last + 1]);
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