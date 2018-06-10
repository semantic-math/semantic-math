type glyph = {
  advance: float,
  bearingX: float,
  bearingY: float,
  height: float,
  width: float,
};

type font_data = {
  mutable unitsPerEm: float,
  mutable glyphMetrics: Js.Dict.t(glyph),
};

type glyph_metrics = Js.Dict.t(glyph);

let fontData: font_data = {
  unitsPerEm: 0.,
  glyphMetrics: Js.Dict.empty(),
};

let decodeGlyph = json => {
  open! Json.Decode;
  {
    advance: json |> field("advance", float),
    bearingX: json |> field("bearingX", float),
    bearingY: json |> field("bearingY", float),
    height: json |> field("height", float),
    width: json |> field("width", float),
  };
};

let decodeFontData = (json: Js.Json.t) => {
  open! Json.Decode;
  fontData.unitsPerEm = json |> field("unitsPerEm", float);
  fontData.glyphMetrics = json |> field("glyphMetrics", dict(decodeGlyph));
};

exception Unhandled;

/* TODO: return width, height, depth metrics */
let getMetrics = (char: Char.t, fontSize: float) => {
  let {unitsPerEm, glyphMetrics} = fontData;
  switch (Js.Dict.get(glyphMetrics, string_of_int(Char.code(char)))) {
  | Some({advance, bearingX, bearingY, width, height}) => 
    {
      advance: fontSize *. advance /. unitsPerEm,
      bearingX: fontSize *. bearingX /. unitsPerEm,
      bearingY: fontSize *. bearingY /. unitsPerEm,
      width: fontSize *. width /. unitsPerEm,
      height: fontSize *. height /. unitsPerEm,
    }
  | None => raise(Unhandled)
  }
};

let getCharWidth = (char: Char.t, fontSize: float) => {
  let {unitsPerEm, glyphMetrics} = fontData;
  switch (Js.Dict.get(glyphMetrics, string_of_int(Char.code(char)))) {
  | Some({advance}) => fontSize *. advance /. unitsPerEm
  | None => raise(Unhandled)
  }
};

let getCharHeight = (char: Char.t, fontSize: float) => {
  let {unitsPerEm, glyphMetrics} = fontData;
  switch (Js.Dict.get(glyphMetrics, string_of_int(Char.code(char)))) {
  | Some({bearingY}) => fontSize *. bearingY /. unitsPerEm
  | None => raise(Unhandled)
  }
};

let getCharDepth = (char: Char.t, fontSize: float) => {
  let {unitsPerEm, glyphMetrics} = fontData;
  switch (Js.Dict.get(glyphMetrics, string_of_int(Char.code(char)))) {
  | Some({bearingY, height}) => fontSize *. (height -. bearingY) /. unitsPerEm;
  | None => raise(Unhandled)
  }
};
