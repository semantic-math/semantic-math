/**
 * Typesetter - Converts a math AST into a Layout
 */

type typesetter = {
  typeset: Node.t => Layout.node
};

/* fontMetrics: Metrics.font_data */
let make = (~baseFontSize=30., metrics: Metrics.metrics): typesetter => {
  let rec typeset =
        (~fontScale=1.0, node: Node.t): Layout.node => {
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
                    Glyph('(', fontSize, metrics),
                    typeset(~fontScale, arg),
                    Glyph(')', fontSize, metrics),
                  ]),
                )
              | _ => typeset(~fontScale, arg)
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
                Glyph(lop, fontSize, metrics),
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
                    Glyph('(', fontSize, metrics),
                    typeset(~fontScale, arg),
                    Glyph(')', fontSize, metrics),
                  ]),
                ) :
                typeset(~fontScale, arg);
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
      let num = hpackNat([typeset(num)]);
      let den = hpackNat([typeset(den)]);
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
          typeset(~fontScale, base),
          Box(
            -. (xHeight *. baseFontSize *. expFontScale),
            hpackNat([typeset(~fontScale=expFontScale, exp)]),
          ),
        ]),
      );
    | Node.Apply(Node.Neg, args) =>
      switch (List.hd(args)) {
      | Node.Apply(Node.Add | Node.Sub, _) =>
        Box(
          0.,
          hpackNat([
            Glyph('-', fontSize, metrics),
            Glyph('(', fontSize, metrics),
            typeset(~fontScale, List.hd(args)),
            Glyph(')', fontSize, metrics),
          ]),
        )
      | _ =>
        Box(
          0.,
          hpackNat([
            Glyph('-', fontSize, metrics),
            typeset(~fontScale, List.hd(args)),
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
              Glyph(Js.String.fromCharCode(0x221A).[0], fontSize, metrics),
              Glyph('(', fontSize, metrics),
              typeset(~fontScale, List.hd(args)),
              Glyph(')', fontSize, metrics),
            ]),
          )
        | _ =>
          Box(
            0.,
            hpackNat([
              typeset(~fontScale, func),
              Glyph('(', fontSize, metrics),
              typeset(~fontScale, List.hd(args)),
              Glyph(')', fontSize, metrics),
            ]),
          )
        }
      | _ =>
        Box(
          0.,
          hpackNat([
            typeset(~fontScale, func),
            Glyph('(', fontSize, metrics),
            typeset(~fontScale, List.hd(args)),
            Glyph(')', fontSize, metrics),
          ]),
        )
      }
    | Node.Apply(op, args) when op == Node.Eq =>
        let boxList =
        List.fold_left(
          (acc, arg) => {
            switch (acc) {
            | [] => [typeset(~fontScale, arg)]
            | _ =>
              acc
              @ [
                Kern(spaceSize *. fontScale),
                Glyph('=', fontSize, metrics),
                Kern(spaceSize *. fontScale),
                typeset(~fontScale, arg),
              ]
            };
          },
          [],
          args,
        );
      Box(0., hpackNat(boxList));
    | Node.Number(value) =>
      Box(
        0.,
        hpackNat(
          Array.to_list(
            Array.map(
              (c: string) => Glyph(c.[0], fontSize, metrics),
              Js.String.split("", value),
            ),
          ),
        ),
      )
    | Node.Identifier(value) =>
      switch (value) {
      | "pi" => Glyph(Js.String.fromCharCode(0x03c0).[0], fontSize, metrics)
      | "theta" => Glyph(Js.String.fromCharCode(0x03b8).[0], fontSize, metrics)
      | _ =>
        Box(
          0.,
          hpackNat(
            Array.to_list(
              Array.map(
                (c: string) => Glyph(c.[0], fontSize, metrics),
                Js.String.split("", value),
              ),
            ),
          ),
        )
      }
    | _ => Kern(0.)
    };
  };
    
  let typesetter = {
    typeset: typeset,
  };
  typesetter;
};
