/**
 * Based on:
 * - https://people.eecs.berkeley.edu/~fateman/temp/neuform.pdf
 * - https://github.com/pkamenarsky/formulae
 */
open Metrics;

type boxkind =
  | VBox
  | HBox;

type dist = float;
type dim = {
  width: dist,
  depth: dist,
  height: dist,
};

type inf_order = 
  | Normal 
  | Fil 
  | Fill 
  | Filll;

type glue_param =
  | Natural
  | Stretching(float, inf_order)
  | Shrinking(float, inf_order)

type glue_spec = {
  size: dist,
  stretch: (dist, inf_order),
  shrink: (dist, inf_order),
};

type node =
  | Glyph(char, float, metrics) /* TODO: add 'font' as a param */
  | Kern(dist)
  | Rule(dim)
  | Box(dist, box)
  | Glue(glue_spec)
and box = {
  kind: boxkind,
  width: dist,
  depth: dist,
  height: dist,
  content: list(node),
};

type hlist = list(node);

type vlist = list(node);

let width = a =>
  switch (a) {
  | Glyph(char, fontSize, metrics) => metrics.getCharWidth(char, fontSize)
  | Kern(size) => size
  | Rule({width}) => width
  | Box(_, {width}) => width
  | Glue({size}) => size
  };

let height = a => {
  switch (a) {
  | Glyph(char, fontSize, metrics) => metrics.getCharHeight(char, fontSize)
  | Rule({height}) => height
  | Box(shift, {height}) => height -. shift
  | _ => 0.
  };
};

let depth = a =>
  switch (a) {
  | Glyph(char, fontSize, metrics) => metrics.getCharDepth(char, fontSize)
  | Rule({depth}) => depth
  | Box(shift, {depth}) => depth +. shift
  | _ => 0.
  };

let vwidth = a =>
  switch (a) {
  | Glyph(char, fontSize, metrics) => metrics.getCharWidth(char, fontSize)
  | Rule({width}) => width
  | Box(shift, {width}) => width +. shift
  | _ => 0.
  };

let vsize = a =>
  switch (a) {
  | Glyph(char, fontSize, metrics) => metrics.getMetrics(char, fontSize).height
  | Rule({height, depth}) => height +. depth
  | Box(_, {height, depth}) => height +. depth
  | Kern(size) => size
  | Glue({size}) => size
  };

let sum = List.fold_left((+.), 0.);
let max = List.fold_left(Js.Math.max_float, 0.);

let compute = (f, g, nl) => nl |> List.map(g) |> f;

let hlistWidth = compute(sum, width);
let hlistHeight = compute(max, height);
let hlistDepth = compute(max, depth);
let vlistWidth = compute(max, vwidth);
let vlistVsize = compute(sum, vsize);

let makebox = (kind, {height, depth, width}: dim, content) => {
  kind,
  width,
  height,
  depth,
  content,
};

let hbox = makebox(HBox);
let vbox = makebox(VBox);

let hpackNat = nl => 
  hbox(
    {
      width: hlistWidth(nl),
      height: hlistHeight(nl),
      depth: hlistDepth(nl),
    },
    nl,
  );

let box0 = (box) => Box(0., box);

let makeList = (dist, box) => [Kern(dist), box0(box)];

let makeVBox = (width, node, upList, dnList) => {
  let height = vlistVsize(upList) +. height(node);
  let depth = vlistVsize(dnList) +. depth(node);
  let nodeList = List.rev_append(upList, [node, ...dnList]);
  /* Js.log("height = " ++ string_of_float(height));
  Js.log("depth = " ++ string_of_float(depth)); */
  vbox({width, depth, height}, nodeList);
}

let fil = (1., Fil);
let fill = (1., Fill);
let filll = (1., Filll);
let ssGlue = {size: 0., stretch: fill, shrink: fill};

let rebox = (newWidth, box) => {
  let {kind, width, height, depth, content} = box;
  if (newWidth == width) {
    box;
  } else if (content == []) {
    hpackNat([Kern(newWidth)]);
  } else {
    let hl = switch(kind) {
    | VBox => [Box(0., box)] /* get the shift of the incoming box */
    | HBox => content
    };
    let glue = Glue(ssGlue);
    hbox({width: newWidth, height, depth}, [glue] @ hl @ [glue]);
  };
};

let makeFract = (thickness, width, numBox: box, denBox: box) => {
  Js.log("makeFract");
  let halfThickness = 0.5 *. thickness;
  /* let axisHeight = 10. /* TODO: calculate the midline of '=' */

  /* TODO: figure out what these values should be */
  let axisNum = 10. -. axisHeight;
  let axisDen = 10. +. axisHeight;

  let distNum = axisNum -. halfThickness -. numBox.depth; /* distance between fraction bar and numerator */
  let distDen = axisDen -. halfThickness -. denBox.height; distance between fraction bar and denominator */
  
  let stroke = Rule({height: halfThickness, depth: halfThickness, width});
  let numBox' = rebox(width, numBox);
  let denBox' = rebox(width, denBox);
  makeVBox(width, stroke, makeList(10., numBox'), makeList(10., denBox'));
};
