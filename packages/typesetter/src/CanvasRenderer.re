[%%debugger.chrome];
open Webapi.Canvas;

exception NoBody;
exception NoDocument;

let makeContext = (width: int, totalHeight: int) => {
  open Webapi.Dom;
  let myCanvas = Document.createElement("canvas", document);

  myCanvas |> Element.setAttribute("width", string_of_int(2 * width));
  myCanvas |> Element.setAttribute("height", string_of_int(2 * totalHeight));
  myCanvas
  |> Element.setAttribute(
       "style",
       {j|width:$(width)px; height:$(totalHeight)px; display:block; margin:16px;|j},
     );

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
