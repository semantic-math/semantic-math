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

ctx |. setFillStyle(String, "cyan");
ctx |. fillRect(~x=0., ~y=0., ~w=100., ~h=100.);

ctx |. setFillStyle(String, "#000000");
ctx |. font("100px comic sans ms");
/* ctx |> fillText("Hello, world!", ~x=50., ~y=100.); */

ctx |. lineCap(LineCap.round);
ctx |. lineWidth(10.);
ctx |. setStrokeStyle(String, "magenta");

ctx |> beginPath;
ctx |> moveTo(~x=100., ~y=100.);
ctx |> lineTo(~x=200., ~y=200.);
ctx |> stroke;

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

       /* TODO: draw bounding boxes around each letter in "Hello, world!" */
       /* let text = "Hello, world!";
          let pen = {
            x: 50.,
            y: 100.,
          };
          let fontSize = 100.;
          ctx |. lineWidth(1.);
          text |> String.iter(letter => {
            let {width, height, advance, bearingX, bearingY} = getMetrics(letter, fontSize);
            Js.log(pen);
            ctx |. strokeRect(~x=pen.x +. bearingX, ~y=pen.y -. bearingY -. height, ~w=width, ~h=height);
            ctx |> fillText(String.make(1, letter), ~x=pen.x, ~y=pen.y);
            pen.x = pen.x +. advance;
          }); */

       module MyLayout =
         Layout.Make({
           let getMetrics = getMetrics;
         });

       {
         open MyLayout;
         let nl = [
           Glyph('a', 30.),
           Kern(12.),
           Glyph('+', 30.),
           Kern(12.),
           Glyph('b', 30.),
           Kern(12.),
           Glyph('-', 30.),
           Kern(12.),
           Glyph('c', 30.),
         ];

         let box = hpackNat(nl);

         Js.log("Layout");
         Js.log(box);

         let pen = {x: 50., y: 100.};

         box.content
         |> List.iter(atom =>
              switch (atom) {
              | Glyph(char, size) =>
                ctx |. font("30px comic sans ms");
                ctx |> fillText(String.make(1, char), ~x=pen.x, ~y=pen.y);
                pen.x = pen.x +. getMetrics(char, size).width;
              | Kern(size) => pen.x = pen.x +. size
              | _ => ()
              }
            );
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