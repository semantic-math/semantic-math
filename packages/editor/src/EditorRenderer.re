open Webapi.Canvas;
open Cursor;
open EditorNode;

type point = {
  mutable x: float,
  mutable y: float,
};

exception Invalid_Axis;

let pen = {x:5., y:10.};
let axis = "x";
switch (axis) {
| "x" => pen.x = 10.
| _ => raise(Invalid_Axis)
};

exception NotFound;

let idForNode = node =>
  switch (node) {
  | Box(id, _, _) => id
  | Glyph(id, _) => id
  };

let rec indexOf = (x, lst, c) =>
  switch (lst) {
  | [] => raise(NotFound)
  | [hd, ...tl] => hd == x ? c : indexOf(x, tl, c + 1)
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

let rec renderLayout =
        (
          ctx: Webapi.Canvas.Canvas2d.t,
          layout: EditorLayout.node,
          cursor: cursor,
        ) => {
  let pen = {x: 0., y: 0.};

  /* render cursor */
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
          let pen = {x: pen.x +. EditorLayout.width(layout), y: pen.y};
          drawCursor(ctx, pen, 60.);
        };
      | _ => ()
      }
    }
  | _ => ()
  };

  switch (layout) {
  | (id, Box(shift, {kind, children, ascent: parentAscent})) =>
    switch (kind) {
    | EditorLayout.HBox =>
      Canvas2dRe.save(ctx);
      Canvas2dRe.translate(~x=0., ~y=-. shift, ctx);

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
          pen.x = pen.x +. EditorLayout.width(child);
        },
        children,
      );
      Canvas2dRe.restore(ctx);
    | EditorLayout.VBox =>
      List.iter(
        child =>
          switch (child) {
          | (_, EditorLayout.Kern(size)) => pen.y = pen.y +. size
          | _ =>
            pen.y = pen.y +. EditorLayout.ascent(child);
            Canvas2dRe.save(ctx);
            Canvas2dRe.translate(
              ~x=pen.x,
              ~y=pen.y -. parentAscent -. shift,
              ctx,
            );
            renderLayout(ctx, child, cursor);
            Canvas2dRe.restore(ctx);
            pen.y = pen.y +. EditorLayout.descent(child);
          },
        children,
      )
    }
  | (_, Glyph(char, size, _)) => drawChar(ctx, pen, char, size)
  | (_, Rule({width, ascent, descent})) =>
    Canvas2dRe.fillRect(
      ~x=pen.x,
      ~y=pen.y -. ascent,
      ~w=pen.x +. width,
      ~h=ascent +. descent,
      ctx,
    )
  | _ => ()
  };
};