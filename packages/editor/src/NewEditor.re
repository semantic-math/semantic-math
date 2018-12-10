Js.log("NewEditor");

open Webapi.Canvas;
open Webapi.Dom;
open Metrics;
open UniqueId;

type point = {
  mutable x: float,
  mutable y: float,
};

type node =
  | Row(int, list(node))
  | Sup(int, list(node))
  | Sub(int, list(node))
  | Glyph(int, char);

let ctx = CanvasRenderer.makeContext(1000, 600);

let cursorId = ref(genId());
let cursor = ref(7);

let ast =
  ref(
    Row(
      cursorId^,
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
  | Row(id, children) =>
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
      let row = Row(id, newChildren);
      visitor(row);
    } else if (List.length(newChildren) == 1) {
      Some(List.hd(newChildren));
    } else {
      None;
    };
  | _ => visitor(node)
  };

let rec traverse = (visitor: node => unit, node): unit => {
  switch (node) {
  | Row(_, children) => List.iter(traverse(visitor), children)
  | Sup(_, children) => List.iter(traverse(visitor), children)
  | Sub(_, children) => List.iter(traverse(visitor), children)
  | _ => ()
  };
  visitor(node);
};

exception Unhandled;

let rec remove_at = (n: int, l: list('a)) =>
  switch (l) {
  | [] => []
  | [h, ...t] => n == 0 ? t : [h, ...remove_at(n - 1, t)]
  };

let rec insert_at = (n: int, x: 'a, l: list('a)) =>
  switch (l) {
  | [] => []
  | [h, ...t] => n == 0 ? [h, x, ...t] : [h, ...insert_at(n - 1, x, t)]
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

         Js.log(cursor^);

         /* typeset stuff */
         let pen = {x: 0., y: 100.};
         let rec render = node =>
           switch (node) {
           | Sup(id, children) =>
             List.iteri(
               (i, child) => {
                 if (cursor^ == i && cursorId^ == id) {
                   ctx
                   |> Canvas2d.fillRect(
                        ~x=pen.x,
                        ~y=pen.y -. 0.85 *. 60.,
                        ~w=2.,
                        ~h=60.,
                      );
                 };
                 switch (child) {
                 | Glyph(_, c) =>
                   ctx
                   |> Canvas2d.fillText(
                        String.make(1, c),
                        ~x=pen.x,
                        ~y=pen.y,
                      );
                   pen.x = pen.x +. metrics.getCharWidth(c, 60.);
                 | Sup(_, _) =>
                   pen.y = pen.y -. 30.;
                   render(child);
                   pen.y = pen.y +. 30.;
                 | _ => ignore()
                 };
               },
               children,
             );
             if (cursor^ == List.length(children) && cursorId^ == id) {
               ctx
               |> Canvas2d.fillRect(
                    ~x=pen.x,
                    ~y=pen.y -. 0.85 *. 60.,
                    ~w=2.,
                    ~h=60.,
                  );
             };
           | Row(id, children) =>
             List.iteri(
               (i, child) => {
                 if (cursor^ == i && cursorId^ == id) {
                   ctx
                   |> Canvas2d.fillRect(
                        ~x=pen.x,
                        ~y=pen.y -. 0.85 *. 60.,
                        ~w=2.,
                        ~h=60.,
                      );
                 };
                 switch (child) {
                 | Glyph(_, c) =>
                   ctx
                   |> Canvas2d.fillText(
                        String.make(1, c),
                        ~x=pen.x,
                        ~y=pen.y,
                      );
                   pen.x = pen.x +. metrics.getCharWidth(c, 60.);
                 | Sup(_, _) =>
                   pen.y = pen.y -. 30.;
                   render(child);
                   pen.y = pen.y +. 30.;
                 | _ => ignore()
                 };
               },
               children,
             );
             if (cursor^ == List.length(children) && cursorId^ == id) {
               ctx
               |> Canvas2d.fillRect(
                    ~x=pen.x,
                    ~y=pen.y -. 0.85 *. 60.,
                    ~w=2.,
                    ~h=60.,
                  );
             };
           | _ => ignore()
           };

         render(ast^);
       };

       update();

       Document.addKeyDownEventListener(
         event => {
           let key = KeyboardEvent.key(event);
           Js.log(key);
           switch (key) {
           | "Meta"
           | "Shift"
           | "Alt"
           | "Control" => ignore()
           | "Backspace" =>
             if (cursor^ > 0) {
               ast :=
                 (
                   switch (ast^) {
                   | Row(id, children) =>
                     Row(id, remove_at(cursor^ - 1, children))
                   | _ => raise(Unhandled)
                   }
                 );
               cursor := cursor^ - 1;
             }
           | "ArrowLeft" =>
            traverse(
               node =>
                 switch (node) {
                 | Row(id, children) =>
                   if (id == cursorId^) {
                     let child = List.nth(children, cursor^ - 1)
                     switch (child) {
                     | Glyph(_, _) =>
                       cursor := Js_math.max_int(0, cursor^ - 1);
                     | Sup(id, children) =>
                       cursor := List.length(children);
                       cursorId := id;
                     | _ => raise(Unhandled)
                     };
                   }
                 | Sup(id, children) =>
                   if (id == cursorId^) {
                     let child = List.nth(children, cursor^ - 1);
                     switch (child) {
                     | Glyph(_, _) =>
                       cursor := Js_math.max_int(0, cursor^ - 1);
                     | Sup(id, children) =>
                       cursor := List.length(children);
                       cursorId := id;
                     | _ => raise(Unhandled)
                     };
                   }
                 | Glyph(_, _) => ()
                 | _ => raise(Unhandled)
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
               node =>
                 switch (node) {
                 | Row(id, children) =>
                   if (id == cursorId^) {
                     let child = List.nth(children, cursor^);
                     switch (child) {
                     | Glyph(_, _) =>
                       cursor :=
                         Js_math.min_int(List.length(children), cursor^ + 1)
                     | Sup(id, _) =>
                       cursor := 0;
                       cursorId := id;
                     | _ => raise(Unhandled)
                     };
                   }
                 | Sup(id, children) =>
                   if (id == cursorId^) {
                     let child = List.nth(children, cursor^);
                     switch (child) {
                     | Glyph(_, _) =>
                       cursor :=
                         Js_math.min_int(List.length(children), cursor^ + 1)
                     | Sup(id, _) =>
                       cursor := 0;
                       cursorId := id;
                     | _ => raise(Unhandled)
                     };
                   }
                 | Glyph(_, _) => ()
                 | _ => raise(Unhandled)
                 },
               ast^,
             )
           | "^" =>
             let power =
               Sup(genId(), [Glyph(genId(), '2'), Glyph(genId(), '4')]);
             ast :=
               (
                 switch (ast^) {
                 | Row(id, children) =>
                   Row(id, insert_at(cursor^ - 1, power, children))
                 | _ => raise(Unhandled)
                 }
               );
             Js.log(ast);
             cursor := cursor^ + 1;
           | _ =>
             ast :=
               (
                 switch (ast^) {
                 | Row(id, children) =>
                   Row(
                     id,
                     insert_at(
                       cursor^ - 1,
                       Glyph(genId(), key.[0]),
                       children,
                     ),
                   )
                 | _ => raise(Unhandled)
                 }
               );
             cursor := cursor^ + 1;
           };

           update();
         },
         document,
       );

       resolve();
     })
);