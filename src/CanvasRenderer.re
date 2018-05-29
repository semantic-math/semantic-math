
open Webapi.Canvas;

open Canvas2d;

exception NoBody;
exception NoDocument;

let makeContext = () => Webapi.Dom.({
  let myCanvas = Document.createElement("canvas", document);

  myCanvas |> Element.setAttribute("width", "1600");
  myCanvas |> Element.setAttribute("height", "1200");
  myCanvas |> Element.setAttribute("style", "width:800px; height:600px;");

  switch (Document.asHtmlDocument(document)) {
  | Some(htmlDocument) => 
    switch (HtmlDocument.body(htmlDocument)) {
    | Some(body) => body |> Element.appendChild(myCanvas);
    | _ => raise(NoBody)
    }
  | _ => raise(NoDocument)
  };

  CanvasElement.getContext2d(myCanvas);
});

let ctx = makeContext();

/* enable retina mode */

ctx |> scale(~x=2., ~y=2.);

ctx |. setFillStyle(String, "cyan");
ctx |. fillRect(~x=0., ~y=0., ~w=100., ~h=100.);

ctx |. setFillStyle(String, "#000000");
ctx |. font("100px comic sans ms");
ctx |> fillText("Hello, world!", ~x=50., ~y=100.);

ctx |. lineCap(LineCap.round);
ctx |. lineWidth(10.);
ctx |. setStrokeStyle(String, "magenta");

ctx |> beginPath;
ctx |> moveTo(~x=100., ~y=100.);
ctx |> lineTo(~x=200., ~y=200.);
ctx |> stroke;

module Int = {
    type t = int;
    let compare(a, b) = Pervasives.compare(a, b);
};

module IntMap = Map.Make(Int);

let glyphMap = IntMap.empty;
let glyphMap = IntMap.add(10, "Hello", glyphMap);
let glyphMap = IntMap.add(15, "Goodbye", glyphMap);

IntMap.iter((key, value) => Js.log(string_of_int(key) ++ " = " ++ value), glyphMap);

type point = {
  mutable x: float,
  mutable y: float,
};

Js.Promise.(
  Fetch.fetch("/metrics/comic-sans.json")
  |> then_(Fetch.Response.json)
  |> then_(json => Metrics.({
    let fontData = decodeFontData(json);
    Js.log(fontData.unitsPerEm);
    Js.log(fontData.glyphMetrics);
    /* TODO: draw bounding boxes around each letter in "Hello, world!" */
    let text = "Hello, world!";
    let pen = {
      x: 50.,
      y: 100.,
    };
    let fontSize = 100.;
    ctx |. lineWidth(1.);
    ctx |. setFillStyle(String, "#0000FF");
    text |> String.iter(letter => {
      let {width, height, advance, bearingX, bearingY} = getMetrics(fontData, letter, fontSize);
      Js.log(pen);
      ctx |. strokeRect(~x=pen.x +. bearingX, ~y=pen.y -. bearingY -. height, ~w=width, ~h=height);
      ctx |> fillText(String.make(1, letter), ~x=pen.x, ~y=pen.y);
      pen.x = pen.x +. advance;
    });
    resolve();
  }))
);

let tokens = Lexer.lex("1+2+3");
let ast = MathParser.parse(tokens);

let layout = (node) => {
  switch (node) {
  | Node.Apply(_, _) => ()
  | Identifier(_) => ()
  | Number(_) => ()
  | Ellipses => ()
  }
};

Js.log(Node.toString(ast));
