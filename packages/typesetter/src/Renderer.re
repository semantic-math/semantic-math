type point = {
  mutable x: float,
  mutable y: float,
};

let debug = false;

/* TODO: create a new pen each time and use ctx's save() and restore() methods */
let rec render = (ctx, (_, box), metrics) => {
  open Layout;
  open Webapi.Canvas;

  let pen = {x: 0., y: 0.};
  switch (box) {
  | {kind: HBox, width: w, content} =>
    if (debug) {
      ctx
      |> Canvas2d.strokeRect(
           ~x=pen.x,
           ~y=pen.y -. height((None, Box(0., box))),
           ~w=width((None, Box(0., box))),
           ~h=vsize((None, Box(0., box))),
         );
    };
    /* Finish glue calculations as per pg. 77 in the TeXBook */
    let availableSpace = w -. hlistWidth(content);
    content
    |> List.iter(((_, atom)) =>
         switch (atom) {
         | Glyph(char, fontSize, _) =>
           /* TODO: create a Font object that we can get this name from and have
              it store the metrics as well */
           ctx |. Canvas2d.font(string_of_float(fontSize) ++ "0px comic sans ms");
           ctx |> Canvas2d.fillText(String.make(1, char), ~x=pen.x, ~y=pen.y);
           if (debug) {
             ctx
             |> Canvas2d.strokeRect(
                  ~x=pen.x,
                  ~y=pen.y -. height((None, atom)),
                  ~w=width((None, atom)),
                  ~h=vsize((None, atom)),
                );
           };
           pen.x = pen.x +. width((None, atom));
         | Kern(size) => pen.x = pen.x +. size
         | Box(shift, box) =>
           Canvas2dRe.save(ctx);
           Canvas2dRe.translate(~x=pen.x, ~y=pen.y +. shift, ctx);
           render(ctx, (None, box), metrics);
           Canvas2dRe.restore(ctx);
           /* Js.log("width = " ++ string_of_float(width(atom))); */
           pen.x = pen.x +. width((None, atom));
         | Glue(_) => pen.x = pen.x +. availableSpace /. 2.
         /* TODO: rules should affect horizontal layouts */
         | _ => ()
         }
       );
  | {kind: VBox, content} =>
    if (debug) {
      ctx
      |> Canvas2d.strokeRect(
           ~x=pen.x,
           ~y=pen.y -. height((None, Box(0., box))),
           ~w=width((None, Box(0., box))),
           ~h=vsize((None, Box(0., box))),
         );
    };
    pen.y = pen.y -. box.height;
    content
    |> List.iter(((_, atom)) =>
         switch (atom) {
         | Box(shift, box) =>
           pen.y = pen.y +. height((None, atom));
           Canvas2dRe.save(ctx);
           Canvas2dRe.translate(~x=pen.x, ~y=pen.y +. shift, ctx);
           render(ctx, (None, box), metrics);
           Canvas2dRe.restore(ctx);
           pen.y = pen.y +. depth((None, atom));
         | Rule({width: w, height: h, depth: d}) =>
           pen.y = pen.y +. height((None, atom));
           ctx |> Canvas2d.fillRect(~x=pen.x, ~y=pen.y -. h, ~w, ~h=h +. d);
           pen.y = pen.y +. depth((None, atom));
         | Kern(size) => pen.y = pen.y +. size
         | _ => ()
         /* TOOD: handle other atoms */
         }
       );
  };
};