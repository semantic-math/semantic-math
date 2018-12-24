[%%debugger.chrome];
open Metrics;

type boxkind =
  | HBox
  | VBox;

type dist = float;

type dim = {
  width: dist,
  ascent: dist,
  descent: dist,
};

type node_type =
  | Box(dist, box)
  | Glyph(char, float, metrics)
  | Kern(dist)
  | Rule(dim)
and node = (option(int), node_type)
and box = {
  kind: boxkind,
  width: dist,
  ascent: dist,
  descent: dist,
  children: list(node),
};

let width = ((_, t): node) =>
  switch (t) {
  | Box(_, {width}) => width
  | Glyph(char, size, metrics) => metrics.getCharWidth(char, size)
  | Kern(size) => size
  | Rule({width}) => width
  };

let ascent = ((_, t): node) =>
  switch (t) {
  | Box(shift, {ascent}) => ascent +. shift
  | Glyph(char, size, metrics) => metrics.getCharAscent(char, size)
  | Kern(_) => 0.
  | Rule({ascent}) => ascent
  };

let descent = ((_, t): node) =>
  switch (t) {
  | Box(shift, {descent}) => descent -. shift
  | Glyph(char, size, metrics) => metrics.getCharDescent(char, size)
  | Kern(_) => 0.
  | Rule({descent}) => descent
  };  

let vwidth = ((_, t): node) =>
  switch (t) {
  | Box(_, {width}) => width
  | Glyph(char, size, metrics) => metrics.getCharWidth(char, size)
  | Kern(_) => 0.
  | Rule({width}) => width
  };

let vheight = ((_, t): node) =>
  switch (t) {
  | Box(_, {ascent, descent}) => ascent +. descent
  | Glyph(char, size, metrics) =>
    metrics.getCharAscent(char, size) +. metrics.getCharDescent(char, size)
  | Kern(size) => size
  | Rule({ascent, descent}) => ascent +. descent
  };

let sum = List.fold_left((+.), 0.);
let max = List.fold_left(Js.Math.max_float, 0.);

let compute = (f, g, nl) => nl |> List.map(g) |> f;

let hlistWidth = compute(sum, width);
let hlistAscent = compute(max, ascent);
let hlistDescent = compute(max, descent);
let vlistWidth = compute(max, vwidth);
let vlistHeight = compute(sum, vheight);

let hpackNat = (children: list(node)) =>
  {
    kind: HBox,
    width: hlistWidth(children),
    ascent: hlistAscent(children),
    descent: hlistDescent(children),
    children,
  };
