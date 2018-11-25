/**
 * Typesetter - Converts a math AST into a Layout
 */

type typesetter = {
  typeset: Node.node => Layout.node
};

/* fontMetrics: Metrics.font_data */
let make = (~baseFontSize=30., metrics: Metrics.metrics): typesetter => {
  let rec typeset =
        (~fontScale=1.0, node: Node.node): Layout.node => {
    open Layout;
    let fontSize = fontScale *. baseFontSize;
    let spaceSize = 0.2 *. fontSize;
    let xHeight = 0.65;

    let (id, typ) = node;
    switch (typ) {
    | Node.Apply(op, args) when op == Node.Add || op == Node.Sub =>
      let boxList =
        List.fold_left(
          (acc, arg) => {
            let (_, argTyp) = arg;
            let larg =
              switch (argTyp) {
              | Node.Apply(Node.Add | Node.Sub, _) =>
                (None, Box(
                  0.,
                  hpackNat([
                    (None, Glyph('(', fontSize, metrics)),
                    typeset(~fontScale, arg),
                    (None, Glyph(')', fontSize, metrics)),
                  ]),
                ))
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
                (None, Kern(spaceSize *. fontScale)),
                /* TODO: create an id that links back each +/- to the AST node */
                (None, Glyph(lop, fontSize, metrics)),
                (None, Kern(spaceSize *. fontScale)),
                larg,
              ]
            };
          },
          [],
          args,
        );
      (Some(id), Box(0., hpackNat(boxList)));
    | Node.Apply(Node.Mul(`Implicit), args) =>
      let wrapFactors =
        List.exists(
          arg => {
            let (_, argTyp) = arg;
            switch (argTyp) {
            | Node.Apply(Node.Add | Node.Sub, _) => true
            | _ => false
            };
          },
          args,
        );
      let boxList =
        List.fold_left(
          (acc, arg) => {
            let larg =
              wrapFactors ?
                (None, Box(
                  0.,
                  hpackNat([
                    (None, Glyph('(', fontSize, metrics)),
                    typeset(~fontScale, arg),
                    (None, Glyph(')', fontSize, metrics)),
                  ]),
                )) :
                typeset(~fontScale, arg);
            switch (acc) {
            | [] => [larg]
            | _ => acc @ [larg]
            };
          },
          [],
          args,
        );
      (Some(id), Box(0., hpackNat(boxList)));
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
      (Some(id), Box(-18., frac));
    | Node.Apply(Node.Exp, [base, exp]) =>
      let expFontScale =
        switch (fontScale) {
        | 1.0 => 0.7
        | _ => 0.5
        };
      (Some(id), Box(
        0.,
        hpackNat([
          typeset(~fontScale, base),
          (None, Box(
            -. (xHeight *. baseFontSize *. expFontScale),
            hpackNat([typeset(~fontScale=expFontScale, exp)]),
          )),
        ]),
      ));
    | Node.Apply(Node.Neg, args) => {
      let (_, argTyp) = List.hd(args);
      switch (argTyp) {
      | Node.Apply(Node.Add | Node.Sub, _) =>
        (Some(id), Box(
          0.,
          hpackNat([
            (None, Glyph('-', fontSize, metrics)),
            (None, Glyph('(', fontSize, metrics)),
            typeset(~fontScale, List.hd(args)),
            (None, Glyph(')', fontSize, metrics)),
          ]),
        ))
      | _ =>
        (Some(id), Box(
          0.,
          hpackNat([
            (None, Glyph('-', fontSize, metrics)),
            typeset(~fontScale, List.hd(args)),
          ]),
        ))
      };
    }
    | Node.Apply(Node.Func(func), args) => {
      let (_, funcTyp) = func;
      switch (funcTyp) {
      | Node.Identifier(name) =>
        switch (name) {
        | "sqrt" =>
          (Some(id), Box(
            0.,
            hpackNat([
              (None, Glyph(Js.String.fromCharCode(0x221A).[0], fontSize, metrics)),
              (None, Glyph('(', fontSize, metrics)),
              typeset(~fontScale, List.hd(args)),
              (None, Glyph(')', fontSize, metrics)),
            ]),
          ))
        | _ =>
          (Some(id), Box(
            0.,
            hpackNat([
              typeset(~fontScale, func),
              (None, Glyph('(', fontSize, metrics)),
              typeset(~fontScale, List.hd(args)),
              (None, Glyph(')', fontSize, metrics)),
            ]),
          ))
        }
      | _ =>
        (Some(id), Box(
          0.,
          hpackNat([
            typeset(~fontScale, func),
            (None, Glyph('(', fontSize, metrics)),
            typeset(~fontScale, List.hd(args)),
            (None, Glyph(')', fontSize, metrics)),
          ]),
        ))
      };
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
                (None, Kern(spaceSize *. fontScale)),
                (None, Glyph('=', fontSize, metrics)),
                (None, Kern(spaceSize *. fontScale)),
                typeset(~fontScale, arg),
              ]
            };
          },
          [],
          args,
        );
      (Some(id), Box(0., hpackNat(boxList)));
    | Node.Number(value) =>
      (Some(id), Box(
        0.,
        hpackNat(
          Array.to_list(
            Array.map(
              /* TODO: link back to AST node and index within its value */
              (c: string) => (None, Glyph(c.[0], fontSize, metrics)),
              Js.String.split("", value),
            ),
          ),
        ),
      ))
    | Node.Identifier(value) =>
      switch (value) {
      | "pi" => (Some(id), Glyph(Js.String.fromCharCode(0x03c0).[0], fontSize, metrics))
      | "theta" => (Some(id), Glyph(Js.String.fromCharCode(0x03b8).[0], fontSize, metrics))
      | _ =>
        (Some(id), Box(
          0.,
          hpackNat(
            Array.to_list(
              Array.map(
                /* TODO: link back to AST node and index within its value */
                (c: string) => (None, Glyph(c.[0], fontSize, metrics)),
                Js.String.split("", value),
              ),
            ),
          ),
        ))
      }
    | _ => (Some(id), Kern(0.))
    };
  };
    
  let typesetter = {
    typeset: typeset,
  };
  typesetter;
};
