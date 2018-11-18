[%%debugger.chrome];
open Webapi.Canvas;

open Canvas2d;

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

let rec layout =
        (~fontScale=1.0, ~baseFontSize=30., node: Node.t): Layout.node => {
  open Layout;
  let fontSize = fontScale *. baseFontSize;
  let spaceSize = 0.2 *. fontSize;
  let xHeight = 0.65;

  switch (node) {
  | Node.Apply(op, args) when op == Node.Add || op == Node.Sub =>
    let boxList =
      List.fold_left(
        (acc, arg) => {
          let larg =
            switch (arg) {
            | Node.Apply(Node.Add | Node.Sub, _) =>
              Box(
                0.,
                hpackNat([
                  Glyph('(', fontSize),
                  layout(~fontScale, arg),
                  Glyph(')', fontSize),
                ]),
              )
            | _ => layout(~fontScale, arg)
            };
          let lop =
            switch (op) {
            | Node.Add => '+'
            | Node.Sub => '-'
            | _ => '?'
            };
          switch (acc) {
          | [] => [larg]
          | _ =>
            acc
            @ [
              Kern(spaceSize *. fontScale),
              Glyph(lop, fontSize),
              Kern(spaceSize *. fontScale),
              larg,
            ]
          };
        },
        [],
        args,
      );
    Box(0., hpackNat(boxList));
  | Node.Apply(Node.Mul(`Implicit), args) =>
    let wrapFactors =
      List.exists(
        arg =>
          switch (arg) {
          | Node.Apply(Node.Add | Node.Sub, _) => true
          | _ => false
          },
        args,
      );
    let boxList =
      List.fold_left(
        (acc, arg) => {
          let larg =
            wrapFactors ?
              Box(
                0.,
                hpackNat([
                  Glyph('(', fontSize),
                  layout(~fontScale, arg),
                  Glyph(')', fontSize),
                ]),
              ) :
              layout(~fontScale, arg);
          switch (acc) {
          | [] => [larg]
          | _ => acc @ [larg]
          };
        },
        [],
        args,
      );
    Box(0., hpackNat(boxList));
  | Node.Apply(Node.Div, [num, den]) =>
    let num = hpackNat([layout(num)]);
    let den = hpackNat([layout(den)]);
    let frac =
      makeFract(
        0.085 *. baseFontSize,
        Js.Math.max_float(num.Layout.width, den.Layout.width),
        num,
        den,
      );
    Box(-18., frac);
  | Node.Apply(Node.Exp, [base, exp]) =>
    let expFontScale =
      switch (fontScale) {
      | 1.0 => 0.7
      | _ => 0.5
      };
    Box(
      0.,
      hpackNat([
        layout(~fontScale, base),
        Box(
          -. (xHeight *. baseFontSize *. expFontScale),
          hpackNat([layout(~fontScale=expFontScale, exp)]),
        ),
      ]),
    );
  | Node.Apply(Node.Neg, args) =>
    switch (List.hd(args)) {
    | Node.Apply(Node.Add | Node.Sub, _) =>
      Box(
        0.,
        hpackNat([
          Glyph('-', fontSize),
          Glyph('(', fontSize),
          layout(~fontScale, List.hd(args)),
          Glyph(')', fontSize),
        ]),
      )
    | _ =>
      Box(
        0.,
        hpackNat([
          Glyph('-', fontSize),
          layout(~fontScale, List.hd(args)),
        ]),
      )
    }
  | Node.Apply(Node.Func(func), args) =>
    switch (func) {
    | Node.Identifier(name) =>
      switch (name) {
      | "sqrt" =>
        Box(
          0.,
          hpackNat([
            Glyph(Js.String.fromCharCode(0x221A).[0], fontSize),
            Glyph('(', fontSize),
            layout(~fontScale, List.hd(args)),
            Glyph(')', fontSize),
          ]),
        )
      | _ =>
        Box(
          0.,
          hpackNat([
            layout(~fontScale, func),
            Glyph('(', fontSize),
            layout(~fontScale, List.hd(args)),
            Glyph(')', fontSize),
          ]),
        )
      }
    | _ => Kern(0.)
    }
  | Node.Number(value) =>
    Box(
      0.,
      hpackNat(
        Array.to_list(
          Array.map(
            (c: string) => Glyph(c.[0], fontSize),
            Js.String.split("", value),
          ),
        ),
      ),
    )
  | Node.Identifier(value) =>
    switch (value) {
    | "pi" => Glyph(Js.String.fromCharCode(0x03c0).[0], fontSize)
    | _ =>
      Box(
        0.,
        hpackNat(
          Array.to_list(
            Array.map(
              (c: string) => Glyph(c.[0], fontSize),
              Js.String.split("", value),
            ),
          ),
        ),
      )
    }
  | _ => Kern(0.)
  };
};

let renderToCanvas = math => {
  let tokens = Lexer.lex(math);
  let ast = MathParser.parse(tokens);
  let box2 = Layout.hpackNat([layout(ast)]);

  /* TODO: replace with function to get full height */
  let {Layout.width, Layout.height, Layout.depth} = box2;
  let ctx = makeContext(Js_math.ceil(width), Js_math.ceil(height +. depth));

  /* enable retina mode */
  ctx |> scale(~x=2., ~y=2.);

  /* set styles */
  ctx->(setStrokeStyle(String, "magenta"));
  ctx->(setFillStyle(String, "#0000FF"));
  ctx->(lineWidth(1.));

  Canvas2dRe.save(ctx);
  Canvas2dRe.translate(~x=0., ~y=height, ctx);
  Renderer.render(ctx, box2);
  Canvas2dRe.restore(ctx);
};

Js.Promise.(
  Fetch.fetch("/packages/typesetter/metrics/comic-sans.json")
  |> then_(Fetch.Response.json)
  |> then_(json => {
       open Metrics;
       decodeFontData(json);

       Js.log(fontData.unitsPerEm);
       Js.log(fontData.glyphMetrics);

       renderToCanvas("(a)(k^-(1.2x)-j)");
       renderToCanvas("e^-(x^2+y^2)");
       renderToCanvas("xy^2z^3-a^2bsin(2)");
       renderToCanvas("sin(pi)");
       renderToCanvas("sqrt(5)");
       renderToCanvas("pi");
       renderToCanvas("abc");
       renderToCanvas("(a/(x+y)) / (b/(x-y))");

       resolve();
     })
);