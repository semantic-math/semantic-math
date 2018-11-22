let math = "1+x+x^2+x^3";
let tokens = Lexer.lex(math);
let ast = MathParser.parse(tokens);

/**
 * TODO:
 * - traverse the layout
 * - find all leaf nodes
 * - create a mapping between AST nodes and layout nodes
 * - define rules for navigating within layout nodes
 */

let debug = false;

type point = {
  mutable x: float,
  mutable y: float,
};

type rect = {
  x: float,
  y: float,
  w: float,
  h: float,
};

type cursor = {mutable index: int};

let rec flatten = (~dx=0., ~dy=0., box): list(rect) => {
  open Layout;
  let pen = {x: dx, y: dy};

  switch (box) {
  | {kind: HBox, width: w, content} =>
    /* Finish glue calculations as per pg. 77 in the TeXBook */
    let availableSpace = w -. hlistWidth(content);

    List.fold_left(
      (acc: list(rect), atom: Layout.node) =>
        switch (atom) {
        | Glyph(_, _, _) =>
          let rect = {
            x: pen.x,
            y: pen.y -. height(atom),
            w: width(atom),
            h: vsize(atom),
          };
          pen.x = pen.x +. width(atom);
          acc @ [rect];
        | Box(shift, box) =>
          let rects = flatten(~dx=pen.x, ~dy=pen.y +. shift, box);
          pen.x = pen.x +. width(atom);
          acc @ rects;
        | Glue(_) =>
          pen.x = pen.x +. availableSpace /. 2.;
          acc;
        | Kern(size) =>
          pen.x = pen.x +. size;
          acc;
        | Rule({width: w}) =>
          pen.x = pen.x +. w;
          acc;
        },
      [],
      content,
    );

  | {kind: VBox, content} =>
    pen.y = pen.y -. box.height;
    List.fold_left(
      (acc: list(rect), atom: Layout.node) =>
        switch (atom) {
        | Box(shift, box) =>
          pen.y = pen.y +. height(atom);
          let rects = flatten(~dx=pen.x, ~dy=pen.y +. shift, box);
          pen.y = pen.y +. depth(atom);
          acc @ rects;
        | Rule({width: w, height: h, depth: d}) =>
          pen.y = pen.y +. height(atom);
          let rect = {x: pen.x, y: pen.y -. h, w, h: h +. d};
          pen.y = pen.y +. depth(atom);
          acc @ [rect];
        | Kern(size) =>
          pen.y = pen.y +. size;
          acc;
        | _ => acc
        },
      [],
      content,
    );
  };
};

Js.Promise.(
  Fetch.fetch("/packages/typesetter/metrics/comic-sans.json")
  |> then_(Fetch.Response.json)
  |> then_(json => {
       let metrics = Metrics.make(json);
       let typsetter = Typesetter.make(~baseFontSize=200., metrics);

       let renderToCanvas = math => {
         open Webapi.Canvas;
         open Canvas2d;

         let tokens = Lexer.lex(math);
         let ast = MathParser.parse(tokens);
         let layout = Layout.hpackNat([typsetter.typeset(ast)]);

         Js.log(layout);

         /* TODO: replace with function to get full height */
         let {Layout.width, Layout.height, Layout.depth} = layout;
         let ctx =
           CanvasRenderer.makeContext(
             Js_math.ceil(width),
             Js_math.ceil(height +. depth),
           );

         /* enable retina mode */
         ctx |> scale(~x=2., ~y=2.);

         /* TODO: avoid having to pass in the height */
         let flatLayout = flatten(~dy=layout.height, layout);
         open Webapi.Dom;

         Document.addClickEventListener(
           event => {
             let x = float_of_int(MouseEvent.pageX(event)) -. 16.;
             let y = float_of_int(MouseEvent.pageY(event)) -. 16.;

             List.iteri(
               (_, rect) =>
                 if (x > rect.x
                     && x < rect.x
                     +. rect.w
                     && y > rect.y
                     && y < rect.y
                     +. rect.h) {
                   Js.log("intersection in:");
                   let {x, y, w, h} = rect;
                   Js.log({j|x:$(x) y:$(y) w:$(w) h:$(h)|j});
                 },
               flatLayout,
             );

             Js.log({j|x = $(x), y = $(y)|j});
             ();
           },
           document,
         );

         /* highlight bounding boxes of glyphs and rules */
         ctx->(setFillStyle(String, "#FFFF00"));
         List.iter(
           rect => {
             let {x, y, w, h} = rect;
             Js.log({j|x:$(x) y:$(y) w:$(w) h:$(h)|j});
             ctx |> Canvas2d.fillRect(~x, ~y, ~w, ~h);
           },
           flatLayout,
         );

         /* set styles */
         ctx->(setStrokeStyle(String, "magenta"));
         ctx->(setFillStyle(String, "#0000FF"));
         ctx->(lineWidth(1.));

         Canvas2dRe.save(ctx);
         Canvas2dRe.translate(~x=0., ~y=height, ctx);
         Renderer.render(ctx, layout, metrics);
         Canvas2dRe.restore(ctx);

         /**
          * TODO:
          * - when determining where to place the cursor, include the space around oeprators
          */
         let cursor = {index: 0};

         let {x, y, w, h} = Array.of_list(flatLayout)[cursor.index];
         ctx->(setFillStyle(String, "black"));
         ctx |> fillRect(~x, ~y, ~w=10., ~h);

         Document.addKeyDownEventListener(
           event => {
             let key = KeyboardEvent.key(event);
             switch (key) {
             | "ArrowLeft" => cursor.index = max(0, cursor.index - 1)
             | "ArrowRight" =>
               cursor.index = min(List.length(flatLayout) - 1, cursor.index + 1)
             | _ => ()
             };

             ctx->(setFillStyle(String, "#FFFF00"));
             List.iter(
               rect => {
                 let {x, y, w, h} = rect;
                 Js.log({j|x:$(x) y:$(y) w:$(w) h:$(h)|j});
                 ctx |> Canvas2d.fillRect(~x, ~y, ~w, ~h);
               },
               flatLayout,
             );

             ctx->(setStrokeStyle(String, "magenta"));
             ctx->(setFillStyle(String, "#0000FF"));
             ctx->(lineWidth(1.));

             Canvas2dRe.save(ctx);
             Canvas2dRe.translate(~x=0., ~y=height, ctx);
             Renderer.render(ctx, layout, metrics);
             Canvas2dRe.restore(ctx);

             let {x, y, w, h} = Array.of_list(flatLayout)[cursor.index];
             ctx->(setFillStyle(String, "black"));
             ctx |> fillRect(~x, ~y, ~w=10., ~h);

             Js.log(key);
           },
           document,
         );
       };

       renderToCanvas("2x + 5 = 10");

       resolve();
     })
);
/* let layout = CanvasRenderer.layout; */

/* TODO: make sure font metrics are loaded */
/* CanvasRenderer.renderToCanvas("1+x+x^2+x^3"); */

Js.log("hello, world!");