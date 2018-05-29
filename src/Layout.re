/**
 * Based on:
 * - https://people.eecs.berkeley.edu/~fateman/temp/neuform.pdf
 * - https://github.com/pkamenarsky/formulae
 */
module type MetricsProvider = {
  let getMetrics: (Char.t, float) => Metrics.glyph;
};

module Make = (MP: MetricsProvider) => {
  type boxkind =
    | VBox
    | HBox;

  type node =
    | Glyph(char, float)
    | Kern(dist)
    | Rule(dim)
    | Box(dist, box)
  and dist = float
  and dim = {
    width: dist,
    depth: dist,
    height: dist,
  }
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
    | Glyph(char, fontSize) => MP.getMetrics(char, fontSize).advance
    | Kern(size) => size
    | Rule({width}) => width
    | Box(_, {width}) => width
    };

  let height = a =>
    switch (a) {
    | Glyph(char, fontSize) => MP.getMetrics(char, fontSize).bearingY
    | Rule({height}) => height
    | Box(shift, {height}) => height -. shift
    | _ => 0.
    };

  let depth = a =>
    switch (a) {
    | Glyph(char, fontSize) => MP.getMetrics(char, fontSize).height -. MP.getMetrics(char, fontSize).bearingY
    | Rule({depth}) => depth
    | Box(shift, {depth}) => depth +. shift
    | _ => 0.
    };

  let vwidth = a =>
    switch (a) {
    | Glyph(char, fontSize) => MP.getMetrics(char, fontSize).advance
    | Rule({width}) => width
    | Box(shift, {width}) => width +. shift
    | _ => 0.
    };

  let vsize = a =>
    switch (a) {
    | Glyph(char, fontSize) => MP.getMetrics(char, fontSize).height
    | Rule({height, depth}) => height +. depth
    | Box(_, {height, depth}) => height +. depth
    | Kern(size) => size
    };

  let sum = List.fold_left((+.), 0.);
  let max = List.fold_left(Js.Math.max_float, 0.);

  let compute = (f, g, nl) => nl |> List.map(g) |> f;

  let hlistWidth = compute(sum, width);
  let hlistHeight = compute(max, height);
  let hlistDepth = compute(max, depth);
  let vlistWidth = compute(max, vwidth);
  let vlistVsize = compute(sum, vsize);

  let makebox = (kind, {height, depth, width}, content) => {
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
};