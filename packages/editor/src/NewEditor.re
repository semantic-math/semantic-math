[%%debugger.chrome];
Js.log("NewEditor");

open Webapi.Canvas;
open Metrics;
open UniqueId;

type point = {
  mutable x: float,
  mutable y: float,
};

type node =
  | Box(int, kind, list(node))
  | Glyph(int, char)
and kind =
  | Row
  | Sup
  | Sub
  | Frac;

let rec toJson = node => {
  Json.Encode.(
    switch (node) {
    | Box(id, kind, children) =>
      object_([
        ("id", int(id)),
        ("kind", switch(kind) {
        | Row => string("row")
        | Sup => string("sup")
        | Sub => string("sub")
        | Frac => string("frac")
        }),
        ("childern", jsonArray(Array.map(toJson, Array.of_list(children)))),
      ])
    | Glyph(id, char) => 
      object_([
        ("id", int(id)),
        ("char", string(String.make(1, char))),
      ])
    }
  )
}

let ctx = CanvasRenderer.makeContext(1000, 600);

let cursorId = ref(genId());
let cursor = ref(7);

let ast =
  ref(
    Box(
      cursorId^,
      Row,
      [
        Glyph(genId(), '2'),
        Glyph(genId(), 'x'),
        Glyph(genId(), '+'),
        Glyph(genId(), '5'),
        Glyph(genId(), '='),
        Glyph(genId(), '1'),
        Glyph(genId(), '0'),
      ],
    ),
  );

let rec fold_left = (i: int, f: ('a, int, 'b) => 'a, accu: 'a, l: list('b)) =>
  switch (l) {
  | [] => accu
  | [h, ...t] => fold_left(i + 1, f, f(accu, i + 1, h), t)
  };

let fold_left = (f: ('a, int, 'b) => 'a, accu: 'a, l: list('b)) =>
  fold_left(0, f, accu, l);

/* TODO: create function to transform the ast */
type visitor = node => option(node);

let rec transform = (visitor: visitor, node): option(node) =>
  switch (node) {
  | Box(id, kind, children) =>
    let newChildren =
      List.fold_left(
        (acc: list(node), child) =>
          switch (transform(visitor, child)) {
          | Some(node) => acc @ [node]
          | None => acc
          },
        [],
        children,
      );
    if (List.length(newChildren) > 1) {
      let row = Box(id, kind, newChildren);
      visitor(row);
    } else if (List.length(newChildren) == 1) {
      Some(List.hd(newChildren));
    } else {
      None;
    };
  | _ => visitor(node)
  };

let rec traverse =
        (~parent=None, visitor: (node, option(node)) => unit, node): unit => {
  switch (node) {
  | Box(_, _, children) =>
    List.iter(
      child => traverse(~parent=Some(node), visitor, child),
      children,
    )
  | _ => ()
  };
  visitor(node, parent);
};

exception Unhandled;
exception NotFound;

let rec remove_at = (n: int, l: list('a)) =>
  switch (l) {
  | [] => []
  | [h, ...t] => n == 0 ? t : [h, ...remove_at(n - 1, t)]
  };

let rec insert_at = (n: int, x: 'a, l: list('a)) =>
  switch (l) {
  | [] => n == 0 ? [x] : []
  | [h, ...t] => n == 0 ? [x, h, ...t] : [h, ...insert_at(n - 1, x, t)]
  };

let drawCursor = (ctx, pen) =>
  ctx |> Canvas2d.fillRect(~x=pen.x, ~y=pen.y -. 0.85 *. 60., ~w=2., ~h=60.);

let drawChar = (ctx, pen, c) =>
  ctx |> Canvas2d.fillText(String.make(1, c), ~x=pen.x, ~y=pen.y);

let rec indexOf = (x, lst, c) =>
  switch (lst) {
  | [] => raise(NotFound)
  | [hd, ...tl] => hd == x ? c : indexOf(x, tl, c + 1)
  };

Js.Promise.(
  Fetch.fetch("/packages/typesetter/metrics/comic-sans.json")
  |> then_(Fetch.Response.json)
  |> then_(json => {
       let metrics = Metrics.make(json);

       Js.log(json);

       let update = () => {
         /* clear canvas */
         ctx->Canvas2d.setFillStyle(String, "#FFFFFF");
         ctx |> Canvas2d.fillRect(~x=0., ~y=0., ~w=1000., ~h=600.);

         /* set styles */
         ctx->(Canvas2d.font("60px comic sans ms"));
         ctx->Canvas2d.setFillStyle(String, "#000000");

         Js.log({j|cursor = $cursor|j});
         Js.log({j|cursorId = $cursorId|j});

         /* typeset stuff */
         let pen = {x: 0., y: 300.};

         let rec render = node =>
           switch (node) {
           | Box(id, kind, children) =>
             switch (kind) {
             | Sup => pen.y = pen.y -. 30.
             | Sub => pen.y = pen.y +. 30.
             | _ => ()
             };
             List.iteri(
               (i, child) => {
                 if (cursor^ == i && cursorId^ == id) {
                   drawCursor(ctx, pen);
                 };
                 render(child);
               },
               children,
             );
             if (cursor^ == List.length(children) && cursorId^ == id) {
               drawCursor(ctx, pen);
             };
             switch (kind) {
             | Sup => pen.y = pen.y +. 30.
             | Sub => pen.y = pen.y -. 30.
             | _ => ()
             };
           | Glyph(_, c) =>
             drawChar(ctx, pen, c);
             pen.x = pen.x +. metrics.getCharWidth(c, 60.);
           };

         render(ast^);
         Js.log(ast^);
         Js.log(toJson(ast^));
       };

       update();
       open Webapi.Dom;

       Document.addKeyDownEventListener(
         event => {
           let key = KeyboardEvent.key(event);
           let processed = ref(false);
           Js.log(key);
           switch (key) {
           | "Meta"
           | "Shift"
           | "Alt"
           | "Control" => ignore()
           | "Backspace" =>
             if (cursor^ > 0) {
               let newAst =
                 transform(
                   node =>
                     switch (node) {
                     | Box(id, kind, children) =>
                       if (id == cursorId^ && !processed^) {
                         processed := true;
                         Some(
                           Box(id, kind, remove_at(cursor^ - 1, children)),
                         );
                       } else {
                         Some(node);
                       }
                     | _ => Some(node)
                     },
                   ast^,
                 );
               ast :=
                 (
                   switch (newAst) {
                   | Some(node) => node
                   | _ => raise(Unhandled)
                   }
                 );
               if (processed^) {
                cursor := cursor^ - 1;
               }
             }
           | "ArrowLeft" =>
             traverse(
               (node, parent) =>
                 switch (node) {
                 | Box(id, _, children) =>
                   /* we used processed to prevent processing the same event twice */
                   if (id == cursorId^ && ! processed^) {
                     processed := true;
                     if (cursor^ == 0) {
                       switch (parent) {
                       | Some(Box(id, _, children)) =>
                         cursorId := id;
                         cursor := indexOf(node, children, 0);
                         Js.log({j|cursor = $cursor|j});
                       | _ => ()
                       };
                     } else {
                       let child = List.nth(children, cursor^ - 1);
                       switch (child) {
                       | Glyph(_, _) =>
                         cursor := Js_math.max_int(0, cursor^ - 1)
                       | Box(id, _, children) =>
                         cursor := List.length(children);
                         cursorId := id;
                       };
                     };
                   }
                 | Glyph(_, _) => ()
                 },
               ast^,
             )
           /* switch (ast^) {
              | Row(id, _) =>
                if (id == cursorId^) {
                  cursor := Js_math.max_int(0, cursor^ - 1);
                }
              | _ => raise(Unhandled)
              } */
           | "ArrowRight" =>
             traverse(
               (node, parent) =>
                 switch (node) {
                 | Box(id, _, children) =>
                   /* we used processed to prevent processing the same event twice */
                   if (id == cursorId^ && ! processed^) {
                     processed := true;
                     if (cursor^ == List.length(children)) {
                       switch (parent) {
                       | Some(Box(id, _, children)) =>
                         cursorId := id;
                         cursor := indexOf(node, children, 0) + 1;
                         Js.log({j|cursor = $cursor|j});
                       | _ => ()
                       };
                     } else {
                       let child = List.nth(children, cursor^);
                       switch (child) {
                       | Glyph(_, _) =>
                         cursor :=
                           Js_math.min_int(
                             List.length(children),
                             cursor^ + 1,
                           )
                       | Box(id, _, _) =>
                         cursor := 0;
                         cursorId := id;
                       };
                     };
                   }
                 | Glyph(_, _) => ()
                 },
               ast^,
             )
           | "^" =>
             let powerId = genId();
             let power =
               Box(
                 powerId,
                 Sup,
                 [Glyph(genId(), '2'), Glyph(genId(), '4')],
               );
             let newAst =
               transform(
                 node =>
                   switch (node) {
                   | Box(id, kind, children) =>
                     if (id == cursorId^) {
                       let newChildren =
                         insert_at(cursor^, power, children);
                       Some(Box(id, kind, newChildren));
                     } else {
                       Some(node);
                     }
                   | _ => Some(node)
                   },
                 ast^,
               );
             ast :=
               (
                 switch (newAst) {
                 | Some(node) => node
                 | _ => raise(Unhandled)
                 }
               );
             cursorId := powerId;
             cursor := 2;
           | _ =>
             let newAst =
               transform(
                 node =>
                   switch (node) {
                   | Box(id, kind, children) =>
                     if (id == cursorId^ && !processed^) {
                       processed := true;
                       let newChildren =
                         insert_at(
                           cursor^,
                           Glyph(genId(), key.[0]),
                           children,
                         );
                       cursor := cursor^ + 1;
                       Some(Box(id, kind, newChildren));
                     } else {
                       Some(node);
                     }
                   | _ => Some(node)
                   },
                 ast^,
               );
             ast :=
               (
                 switch (newAst) {
                 | Some(node) => node
                 | _ => raise(Unhandled)
                 }
               );
           };

           update();
         },
         document,
       );

       resolve();
     })
);