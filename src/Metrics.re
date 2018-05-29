type glyph = {
  advance: float,
  bearingX: float,
  bearingY: float,
  height: float,
  width: float,
};

type font_data = {
  unitsPerEm: float,
  glyphMetrics: Js.Dict.t(glyph),
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
  {
    unitsPerEm: json |> field("unitsPerEm", float),
    glyphMetrics: json |> field("glyphMetrics", dict(decodeGlyph)),
  };
};

exception Unhandled;

/* TODO: return width, height, depth metrics */
let getMetrics = (fontData: font_data, char: Char.t, fontSize: float) => {
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
