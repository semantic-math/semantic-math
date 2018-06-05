[%%debugger.chrome];
open Webapi.Canvas;

open Canvas2d;

exception NoBody;
exception NoDocument;

let makeContext = () => {
  open Webapi.Dom;
  let myCanvas = Document.createElement("canvas", document);

  myCanvas |> Element.setAttribute("width", "1600");
  myCanvas |> Element.setAttribute("height", "1200");
  myCanvas |> Element.setAttribute("style", "width:800px; height:600px;");

  switch (Document.asHtmlDocument(document)) {
  | Some(htmlDocument) =>
    switch (HtmlDocument.body(htmlDocument)) {
    | Some(body) => body |> Element.appendChild(myCanvas)
    | _ => raise(NoBody)
    }
  | _ => raise(NoDocument)
  };

  CanvasElement.getContext2d(myCanvas);
};

let ctx = makeContext();

/* enable retina mode */
ctx |> scale(~x=2., ~y=2.);

ctx |. setFillStyle(String, "#000000");
ctx |. setStrokeStyle(String, "magenta");

module Int = {
  type t = int;
  let compare = (a, b) => Pervasives.compare(a, b);
};

module IntMap = Map.Make(Int);

let glyphMap = IntMap.empty;
let glyphMap = IntMap.add(10, "Hello", glyphMap);
let glyphMap = IntMap.add(15, "Goodbye", glyphMap);

IntMap.iter(
  (key, value) => Js.log(string_of_int(key) ++ " = " ++ value),
  glyphMap,
);

type point = {
  mutable x: float,
  mutable y: float,
};

Js.Promise.(
  Fetch.fetch("/metrics/comic-sans.json")
  |> then_(Fetch.Response.json)
  |> then_(json => {
       open Metrics;
       let fontData = decodeFontData(json);
       /* partial application so we don't have to pass fontData around */
       let getMetrics = getMetrics(fontData);

       Js.log(fontData.unitsPerEm);
       Js.log(fontData.glyphMetrics);

       ctx |. setFillStyle(String, "#0000FF");

       module MyLayout =
         Layout.Make({
           let getMetrics = getMetrics;
           let getCharWidth = getCharWidth(fontData);
           let getCharHeight = getCharHeight(fontData);
           let getCharDepth = getCharDepth(fontData);
         });

       let fontSize = 60.;

       {
         open MyLayout;

         let num = hpackNat([Glyph('1', fontSize)]);
         let den =
           hpackNat([
             Glyph('x', fontSize),
             Kern(12.),
             Glyph('+', fontSize),
             Kern(12.),
             Glyph('y', fontSize),
           ]);

         /* We have to use .MyLayout.width here b/c Metrics always has a .width prop */
         let fract =
           makeFract(
             4.5,
             Js.Math.max_float(num.MyLayout.width, den.MyLayout.width),
             num,
             den,
           );

         let nl = [
           Glyph('(', fontSize),
           Glyph('a', fontSize),
           Kern(12.),
           Glyph('+', fontSize),
           Kern(12.),
           Glyph('(', fontSize),
           Glyph('b', fontSize),
           Kern(12.),
           Glyph('-', fontSize),
           Kern(12.),
           Glyph('c', fontSize),
           Glyph(')', fontSize),
           Glyph(')', fontSize),
           Kern(12.),
           Glyph('+', fontSize),
           Kern(12.),
           Box(-18., fract),
         ];

         let box = hpackNat(nl);
         let debug = true;

         /* TODO: create a new pen each time and use ctx's save() and restore() methods */
         let rec render = (box, shift) => {
           let pen = {x: 0., y: 0.};
           switch (box) {
           | {kind: HBox, MyLayout.width: w, content} =>
             if (debug) {
               ctx
               |> strokeRect(
                    ~x=pen.x,
                    ~y=pen.y -. height(Box(shift, box)),
                    ~w=MyLayout.width(Box(shift, box)),
                    ~h=MyLayout.vsize(Box(shift, box)),
                  );
             };
             /* Finish glue calculations as per pg. 77 in the TeXBook */
             let availableSpace = w -. hlistWidth(content);
             content
             |> List.iter(atom =>
                  switch (atom) {
                  | Glyph(char, _) =>
                    ctx |. font("60px comic sans ms");
                    ctx |> fillText(String.make(1, char), ~x=pen.x, ~y=pen.y);
                    if (debug) {
                      ctx
                      |> strokeRect(
                           ~x=pen.x,
                           ~y=pen.y -. height(atom),
                           ~w=MyLayout.width(atom),
                           ~h=MyLayout.vsize(atom),
                         );
                    };
                    pen.x = pen.x +. MyLayout.width(atom);
                  | Kern(size) => pen.x = pen.x +. size
                  | Box(shift, box) =>
                    Canvas2dRe.save(ctx);
                    Canvas2dRe.translate(~x=pen.x, ~y=pen.y +. shift, ctx);
                    render(box, shift);
                    Canvas2dRe.restore(ctx);
                  | Glue(_) =>
                    pen.x = pen.x +. availableSpace /. 2.;
                  | _ => ()
                  }
                );
           | {kind: VBox, content} =>
             if (debug) {
               ctx
               |> strokeRect(
                    ~x=pen.x,
                    ~y=pen.y -. height(Box(0., box)),
                    ~w=MyLayout.width(Box(0., box)),
                    ~h=MyLayout.vsize(Box(0., box)),
                  );
             };
             pen.y = pen.y -. box.MyLayout.height;
             content
             |> List.iteri((_, atom) =>
                  switch (atom) {
                  | Box(shift, box) =>
                    pen.y = pen.y +. height(atom);
                    Canvas2dRe.save(ctx);
                    Canvas2dRe.translate(~x=pen.x, ~y=pen.y, ctx);
                    render(box, shift);
                    Canvas2dRe.restore(ctx);
                    pen.y = pen.y +. depth(atom);
                  | Rule({MyLayout.width: w, height: h, depth: d}) =>
                    pen.y = pen.y +. height(atom);
                    ctx |> fillRect(~x=pen.x, ~y=pen.y -. h, ~w, ~h=h +. d);
                    pen.y = pen.y +. depth(atom);
                  | Kern(size) => pen.y = pen.y +. size
                  | _ => ()
                  }
                );
           };
         };

         ctx |. lineWidth(1.);

         Canvas2dRe.save(ctx);
         Canvas2dRe.translate(~x=200., ~y=400., ctx);

         render(box, 0.);
         Canvas2dRe.restore(ctx);

         Canvas2dRe.save(ctx);
         Canvas2dRe.translate(~x=200., ~y=200., ctx);
         render(fract, 0.);
         Canvas2dRe.restore(ctx);
       };

       resolve();
     })
);

let tokens = Lexer.lex("1+2+3");
let ast = MathParser.parse(tokens);

let layout = node =>
  switch (node) {
  | Node.Apply(_, _) => ()
  | Identifier(_) => ()
  | Number(_) => ()
  | Ellipses => ()
  };

Js.log(Node.toString(ast));