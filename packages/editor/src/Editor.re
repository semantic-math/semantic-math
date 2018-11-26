open Node;

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
  id: option(int),
  x: float,
  y: float,
  w: float,
  h: float,
};

/**
 * TODO:
 * - find the leaf nodes in the AST that have ids
 * - find the matching layout node
 * - use the layout node's position to place the cursor
 *
 * - use left/right to navigate between leaf nodes
 * - use number/letter keys to update leaf nodes
 */

let rec foldTree = (visitor, accum, node) => {
  let (_, typ) = node;
  switch (typ) {
  | Node.Apply(_, children) =>
    let result = List.fold_left(foldTree(visitor), accum, children);
    visitor(result, node);
  | _ => visitor(accum, node)
  };
};

let getLeafNodes = ast => {
  let leafNodes =
    foldTree(
      (accum, node) => {
        let (_, typ) = node;
        switch (typ) {
        | Apply(_, _) => accum
        | _ => accum @ [node]
        };
      },
      [],
      ast,
    );
  leafNodes;
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
      (acc: list(rect), (id, atom)) =>
        switch (atom) {
        | Glyph(_, _, _) =>
          let rect = {
            id,
            x: pen.x,
            y: pen.y -. height((None, atom)),
            w: width((None, atom)),
            h: vsize((None, atom)),
          };
          pen.x = pen.x +. width((None, atom));
          acc @ [rect];
        | Box(shift, box) =>
          let rects = flatten(~dx=pen.x, ~dy=pen.y +. shift, box);
          pen.x = pen.x +. width((None, atom));
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
      (acc: list(rect), (id, atom)) =>
        switch (atom) {
        | Box(shift, box) =>
          pen.y = pen.y +. height((None, atom));
          let rects = flatten(~dx=pen.x, ~dy=pen.y +. shift, box);
          pen.y = pen.y +. depth((None, atom));
          acc @ rects;
        | Rule({width: w, height: h, depth: d}) =>
          pen.y = pen.y +. height((None, atom));
          let rect = {id, x: pen.x, y: pen.y -. h, w, h: h +. d};
          pen.y = pen.y +. depth((None, atom));
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

         let tokens = Lexer.lex(math);
         let ast = ref(MathParser.parse(tokens));
         let leafNodes = ref(getLeafNodes(ast^));
         List.iter(node => Js.log(Node.toString(node)), leafNodes^);
         let layout = Layout.hpackNat([typsetter.typeset(ast^)]);

         Js.log(layout);

         /* TODO: replace with function to get full height */
         let {Layout.width, Layout.height, Layout.depth} = layout;
         let ctx =
           CanvasRenderer.makeContext(
             Js_math.ceil(width),
             Js_math.ceil(height +. depth),
           );

         /* enable retina mode */
         ctx |> Canvas2d.scale(~x=2., ~y=2.);

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

             /* Js.log({j|x = $(x), y = $(y)|j}); */
             ();
           },
           document,
         );

         /* highlight bounding boxes of glyphs and rules */
         ctx->(Canvas2d.setFillStyle(String, "#FFFF00"));
         List.iter(
           rect => {
             let {x, y, w, h} = rect;
             /* Js.log({j|x:$(x) y:$(y) w:$(w) h:$(h)|j}); */
             ctx |> Canvas2d.fillRect(~x, ~y, ~w, ~h);
           },
           flatLayout,
         );

         /* set styles */
         ctx->(Canvas2d.setStrokeStyle(String, "magenta"));
         ctx->(Canvas2d.setFillStyle(String, "#0000FF"));
         ctx->(Canvas2d.lineWidth(1.));

         Canvas2dRe.save(ctx);
         Canvas2dRe.translate(~x=0., ~y=height, ctx);
         Renderer.render(ctx, (None, layout), metrics);
         Canvas2dRe.restore(ctx);

         /**
          * TODO:
          * - when determining where to place the cursor, include the space around oeprators
          */
         let cursor = {index: 0};

         let {x, y, h} = Array.of_list(flatLayout)[cursor.index];
         ctx->(Canvas2d.setFillStyle(String, "black"));
         ctx |> Canvas2d.fillRect(~x, ~y, ~w=10., ~h);

         Document.addKeyDownEventListener(
           event => {
             open Node;
             let key = KeyboardEvent.key(event);
             switch (key) {
             | "ArrowLeft" => cursor.index = max(0, cursor.index - 1)
             | "ArrowRight" =>
               cursor.index =
                 min(List.length(flatLayout) - 1, cursor.index + 1)
             | "0"
             | "1"
             | "2"
             | "3"
             | "4"
             | "5"
             | "6"
             | "7"
             | "8"
             | "9" =>
               ast :=
                 Transform.transform(
                   node =>
                     if (node == List.hd(leafNodes^)) {
                       let (id, typ) = node;
                       switch (typ) {
                       | Number(value) => (id, Number(value ++ key))
                       | _ => node
                       };
                     } else {
                       node;
                     },
                   ast^,
                 )
             | "Backspace" =>
               ast :=
                 Transform.transform(
                   node =>
                     if (node == List.hd(leafNodes^)) {
                       let (id, typ) = node;
                       switch (typ) {
                       | Number(value) => (
                           id,
                           Number(
                             String.sub(value, 0, String.length(value) - 1),
                           ),
                         )
                       | _ => node
                       };
                     } else {
                       node;
                     },
                   ast^,
                 )
             | _ => ()
             };

             leafNodes := getLeafNodes(ast^);
             let layout = Layout.hpackNat([typsetter.typeset(ast^)]);

             ctx
             ->(
                 Canvas2d.clearRect(~x=0., ~y=0., ~w=width, ~h=height +. depth)
               );
             ctx->(Canvas2d.setFillStyle(String, "#FFFF00"));
             List.iter(
               rect => {
                 let {x, y, w, h} = rect;
                 /* Js.log({j|x:$(x) y:$(y) w:$(w) h:$(h)|j}); */
                 ctx |> Canvas2d.fillRect(~x, ~y, ~w, ~h);
               },
               flatLayout,
             );

             ctx->(Canvas2d.setStrokeStyle(String, "magenta"));
             ctx->(Canvas2d.setFillStyle(String, "#0000FF"));
             ctx->(Canvas2d.lineWidth(1.));

             Canvas2dRe.save(ctx);
             Canvas2dRe.translate(~x=0., ~y=height, ctx);
             Renderer.render(ctx, (None, layout), metrics);
             Canvas2dRe.restore(ctx);

             let {x, y, h} = Array.of_list(flatLayout)[cursor.index];
             ctx->(Canvas2d.setFillStyle(String, "black"));
             ctx |> Canvas2d.fillRect(~x, ~y, ~w=10., ~h);

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