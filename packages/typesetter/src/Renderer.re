open Layout;
open Webapi.Canvas;

type point = {
  mutable x: float,
  mutable y: float,
};

let debug = true;

/* TODO: create a new pen each time and use ctx's save() and restore() methods */
let rec render = (ctx, box, shift) => {
  let pen = {x: 0., y: 0.};
  switch (box) {
  | {kind: HBox, width: w, content} =>
    if (debug) {
      ctx
      |> Canvas2d.strokeRect(
           ~x=pen.x,
           ~y=pen.y -. height(Box(shift, box)),
           ~w=width(Box(shift, box)),
           ~h=vsize(Box(shift, box)),
         );
    };
    /* Finish glue calculations as per pg. 77 in the TeXBook */
    let availableSpace = w -. hlistWidth(content);
    content
    |> List.iter(atom =>
         switch (atom) {
         | Glyph(char, _) =>
           ctx |. Canvas2d.font("60px comic sans ms");
           ctx |> Canvas2d.fillText(String.make(1, char), ~x=pen.x, ~y=pen.y);
           if (debug) {
             ctx
             |> Canvas2d.strokeRect(
                  ~x=pen.x,
                  ~y=pen.y -. height(atom),
                  ~w=width(atom),
                  ~h=vsize(atom),
                );
           };
           pen.x = pen.x +. width(atom);
         | Kern(size) => pen.x = pen.x +. size
         | Box(shift, box) =>
           Canvas2dRe.save(ctx);
           Canvas2dRe.translate(~x=pen.x, ~y=pen.y +. shift, ctx);
           render(ctx, box, shift);
           Canvas2dRe.restore(ctx);
         | Glue(_) => pen.x = pen.x +. availableSpace /. 2.
         | _ => ()
         }
       );
  | {kind: VBox, content} =>
    if (debug) {
      ctx
      |> Canvas2d.strokeRect(
           ~x=pen.x,
           ~y=pen.y -. height(Box(0., box)),
           ~w=width(Box(0., box)),
           ~h=vsize(Box(0., box)),
         );
    };
    pen.y = pen.y -. box.height;
    content
    |> List.iteri((_, atom) =>
         switch (atom) {
         | Box(shift, box) =>
           pen.y = pen.y +. height(atom);
           Canvas2dRe.save(ctx);
           Canvas2dRe.translate(~x=pen.x, ~y=pen.y, ctx);
           render(ctx, box, shift);
           Canvas2dRe.restore(ctx);
           pen.y = pen.y +. depth(atom);
         | Rule({width: w, height: h, depth: d}) =>
           pen.y = pen.y +. height(atom);
           ctx |> Canvas2d.fillRect(~x=pen.x, ~y=pen.y -. h, ~w, ~h=h +. d);
           pen.y = pen.y +. depth(atom);
         | Kern(size) => pen.y = pen.y +. size
         | _ => ()
         }
       );
  };
};