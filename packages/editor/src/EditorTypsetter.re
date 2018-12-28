open EditorNode;

let flat_map = (fn: 'a => list('b), ls: list('a)): list('b) =>
  List.fold_left((accu, item) => accu @ fn(item), [], ls);

let rec fold_left = (i: int, f: ('a, int, 'b) => 'a, accu: 'a, l: list('b)) =>
  switch (l) {
  | [] => accu
  | [h, ...t] => fold_left(i + 1, f, f(accu, i, h), t)
  };

let fold_left = (f: ('a, int, 'b) => 'a, accu: 'a, l: list('b)) =>
  fold_left(0, f, accu, l);

let flat_mapi = (fn: (int, 'a) => list('b), ls: list('a)): list('b) =>
  fold_left((accu, index, item) => accu @ fn(index, item), [], ls);

let isOperator = node =>
  switch (node) {
  | Glyph(_, c) =>
    let index =
      try (String.index("+-=<>*", c)) {
      | Not_found => (-1)
      };
    index != (-1);
  | _ => false
  };

type typesetter = {typeset: node => EditorLayout.node};

exception UnexpectedNode;

let makeTypesetter = (metrics: Metrics.metrics) => {
  /* TODO: split this into typesetBox and typesetGlyph */

  let rec typeset = (~fontScale=1.0, ast: node): EditorLayout.node =>
    switch (ast) {
    | Box(id, Row, children) =>
      let box = EditorLayout.hpackNat(addSpaces(fontScale, children));
      let size = 60. *. fontScale;
      let minAscent = metrics.getCharAscent('1', size);
      let minDescent = metrics.getCharDescent('y', size);
      (
        Some(id),
        EditorLayout.Box(
          0.,
          {
            kind: box.kind,
            width: box.width,
            ascent: Js.Math.max_float(box.ascent, minAscent),
            descent: Js.Math.max_float(box.descent, minDescent),
            children: box.children,
          },
        ),
      );
    | Box(id, kind, children) when kind == Sup || kind == Sub =>
      let originalFontScale = fontScale;
      let fontScale = fontScale == 1. ? 0.7 : 0.5;
      let box =
        EditorLayout.hpackNat(List.map(typeset(~fontScale), children));
      let size = 60. *. fontScale;
      let minAscent = metrics.getCharAscent('1', size);
      let minDescent = metrics.getCharDescent('y', size);
      (
        Some(id),
        EditorLayout.Box(
          originalFontScale *. (kind == Sup ? 24. : (-20.)),
          {
            kind: box.kind,
            width: box.width,
            ascent: Js.Math.max_float(box.ascent, minAscent),
            descent: Js.Math.max_float(box.descent, minDescent),
            children: box.children,
          },
        ),
      );
    | Box(id, Parens, children) =>
      let childrenWithParens =
        addSpaces(
          fontScale,
          [Glyph(-1, '(')] @ children @ [Glyph(-1, ')')],
        );
      let box = EditorLayout.hpackNat(childrenWithParens);
      let size = 60. *. fontScale;
      let minAscent = metrics.getCharAscent('1', size);
      let minDescent = metrics.getCharDescent('y', size);
      (
        Some(id),
        EditorLayout.Box(
          0.,
          {
            kind: box.kind,
            width: box.width,
            ascent: Js.Math.max_float(box.ascent, minAscent),
            descent: Js.Math.max_float(box.descent, minDescent),
            children: box.children,
          },
        ),
      );
    | Box(id, Frac, children) =>
      switch (children) {
      | [num, den] =>
        let originalFontScale = fontScale;
        let fontScale = fontScale == 1.0 ? 1.0 : 0.5;
        let numLayout = typeset(~fontScale, num);
        let denLayout = typeset(~fontScale, den);
        let width = EditorLayout.vlistWidth([numLayout, denLayout]);
        let numWidth = EditorLayout.width(numLayout);
        let denWidth = EditorLayout.width(denLayout);

        let children = [
          (
            None,
            EditorLayout.Box(
              0.,
              EditorLayout.hpackNat([
                (None, EditorLayout.Kern((width -. numWidth) /. 2.)),
                numLayout,
                (None, EditorLayout.Kern((width -. numWidth) /. 2.)),
              ]),
            ),
          ),
          /* (None, Kern(fontScale *. 8.)), */
          (None, EditorLayout.Rule({width, ascent: 2., descent: 2.})),
          (None, Kern(fontScale *. 8.)),
          (
            None,
            EditorLayout.Box(
              0.,
              EditorLayout.hpackNat([
                (None, EditorLayout.Kern((width -. denWidth) /. 2.)),
                denLayout,
                (None, EditorLayout.Kern((width -. denWidth) /. 2.)),
              ]),
            ),
          ),
        ];
        let ascent = 2. /* +. 8.*/ +. EditorLayout.vheight(numLayout);
        let descent = 2. +. 8. +. EditorLayout.vheight(denLayout);

        (
          Some(id),
          EditorLayout.Box(
            originalFontScale *. 19.,
            {
              kind: EditorLayout.VBox,
              width: EditorLayout.vlistWidth(children),
              ascent,
              descent,
              children,
            },
          ),
        );
      | _ => raise(UnexpectedNode)
      }
    | Glyph(id, c) =>
      let c = c == '*' ? Js.String.fromCharCode(183).[0] : c;
      (Some(id), EditorLayout.Glyph(c, fontScale *. 60., metrics));
    | _ => (None, EditorLayout.Kern(0.))
    }
  and addSpaces = (fontScale, children) =>
    fontScale == 1.0 ?
      flat_mapi(
        (i, child) =>
          switch (child) {
          | Glyph(id, c) =>
            let addSpace =
              if (isOperator(child)) {
                Js.log({j|isOperator = true|j});
                if (i == 0) {
                  false;
                } else {
                  !isOperator(List.nth(children, i - 1));
                };
              } else {
                false;
              };
            Js.log({j|addSpace = $addSpace|j});
            if (addSpace) {
              [
                (
                  Some(id),
                  EditorLayout.Box(
                    0.,
                    EditorLayout.hpackNat([
                      (None, Kern(16.)),
                      (None, Glyph(c, fontScale *. 60., metrics)),
                      (None, EditorLayout.Kern(16.)),
                    ]),
                  ),
                ),
              ];
            } else {
              [typeset(~fontScale, child)];
            };
          | _ => [typeset(~fontScale, child)]
          },
        children,
      ) :
      List.map(typeset(~fontScale), children);

  let typsetter = {typeset: typeset};

  typsetter;
};