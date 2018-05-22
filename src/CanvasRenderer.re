open Webapi.Dom;

open Webapi.Canvas;

open Canvas2d;

exception NoBody;
exception NoDocument;

let myCanvas = Document.createElement("canvas", document);

switch (Document.asHtmlDocument(document)) {
| Some(htmlDocument) => 
  switch (HtmlDocument.body(htmlDocument)) {
  | Some(body) => body |> Element.appendChild(myCanvas);
  | _ => raise(NoBody)
  }
| _ => raise(NoDocument)
};

let ctx = CanvasElement.getContext2d(myCanvas);

/* enable retina mode */
myCanvas |> Element.setAttribute("width", "1600");
myCanvas |> Element.setAttribute("height", "1200");
myCanvas |> Element.setAttribute("style", "width:800px; height:600px;");
ctx |> scale(~x=2., ~y=2.);

ctx |. setFillStyle(String, "cyan");
ctx |. fillRect(~x=0., ~y=0., ~w=100., ~h=100.);

ctx |. setFillStyle(String, "#000000");
ctx |. font("60pt comic sans ms");
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

Js.Promise.(
  Fetch.fetch("/metrics/comic-sans.json")
  |> then_(Fetch.Response.json)
  |> then_(json => Js.log(json) |> resolve)
);
