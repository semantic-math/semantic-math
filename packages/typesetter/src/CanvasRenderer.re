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
  Fetch.fetch("/packages/typesetter/metrics/comic-sans.json")
  |> then_(Fetch.Response.json)
  |> then_(json => {
       open Metrics;
       decodeFontData(json);

       Js.log(fontData.unitsPerEm);
       Js.log(fontData.glyphMetrics);

       ctx |. setFillStyle(String, "#0000FF");

       let fontSize = 60.;

       {
         open Layout;

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
             Js.Math.max_float(num.Layout.width, den.Layout.width),
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

         ctx |. lineWidth(1.);

         Canvas2dRe.save(ctx);
         Canvas2dRe.translate(~x=200., ~y=400., ctx);
         Renderer.render(ctx, box, 0.);
         ctx |. setFillStyle(String, "#000000");
         ctx |. fillRect(~x=0., ~y=0., ~w=5., ~h=5.);
         Canvas2dRe.restore(ctx);

         Canvas2dRe.save(ctx);
         Canvas2dRe.translate(~x=200., ~y=200., ctx);
         Renderer.render(ctx, hpackNat([Box(-18.,fract)]), 0.);
         ctx |. setFillStyle(String, "#000000");
         ctx |. fillRect(~x=0., ~y=0., ~w=5., ~h=5.);
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