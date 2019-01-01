open Tree;

exception Invalid_Tree;

type typesetter = {typeset: tree_node => EditorLayout.node};

let makeTypesetter = (metrics: Metrics.metrics) => {
  let rec typeset = (~fontScale=1.0, ast: tree_node): EditorLayout.node =>
    switch (ast) {
    | (id, Row({children})) =>
      let children = LinkedList.to_list(children);
      let box =
        EditorLayout.hpackNat(List.map(typeset(~fontScale), children));
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
    | (id, SupSub({sup: Some(sup)})) =>
      let originalFontScale = fontScale;
      let fontScale = fontScale == 1. ? 0.7 : 0.5;
      switch (sup.value) {
      | (_, Row({children})) => 
        let children = LinkedList.to_list(children);
        let box =
          EditorLayout.hpackNat(List.map(typeset(~fontScale), children));
        let size = 60. *. fontScale;
        let minAscent = metrics.getCharAscent('1', size);
        let minDescent = metrics.getCharDescent('y', size);
        (
          Some(id),
          EditorLayout.Box(
            originalFontScale *. 24.,
            {
              kind: box.kind,
              width: box.width,
              ascent: Js.Math.max_float(box.ascent, minAscent),
              descent: Js.Math.max_float(box.descent, minDescent),
              children: box.children,
            },
          ),
        );
      | _ => raise(Invalid_Tree)
      }
      
    | (id, Frac({num, den})) =>
      let originalFontScale = fontScale;
      let fontScale = fontScale == 1.0 ? 1.0 : 0.5;
      let numLayout = typeset(~fontScale, num.value);
      let denLayout = typeset(~fontScale, den.value);
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
    | (id, Parens({children})) =>
      let children =
        List.map(typeset(~fontScale), LinkedList.to_list(children));
      let childrenWithParens =
        [(None, EditorLayout.Glyph('(', fontScale *. 60., metrics))]
        @ children
        @ [(None, EditorLayout.Glyph(')', fontScale *. 60., metrics))];

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
    | (id, Glyph({char: c})) =>
      let c = c == '*' ? Js.String.fromCharCode(183).[0] : c;
      (Some(id), EditorLayout.Glyph(c, fontScale *. 60., metrics));
    | _ => (None, EditorLayout.Kern(0.))
    };

  let typsetter = {typeset: typeset};

  typsetter;
};