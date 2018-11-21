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

         /* fill in bounding boxes of glyphs and rules */
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
       };

       renderToCanvas("2x + 5 = 10");
       open Webapi.Dom;

       Document.addClickEventListener(
         event => {
           let x = MouseEvent.pageX(event);
           let y = MouseEvent.pageY(event);

           Js.log({j|x = $(x), y = $(y)|j});
           ();
         },
         document,
       );

       resolve();
     })
);
/* let layout = CanvasRenderer.layout; */

/* TODO: make sure font metrics are loaded */
/* CanvasRenderer.renderToCanvas("1+x+x^2+x^3"); */

Js.log("hello, world!");