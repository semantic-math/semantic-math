Js.log("NewEditor");

open Webapi.Canvas;
open Webapi.Dom;
open Metrics;

type point = {
  mutable x: float,
  mutable y: float,
};

type node =
  | Row(list(node))
  | Sup(list(node))
  | Sub(list(node))
  | Glyph(char);

let ctx = CanvasRenderer.makeContext(1000, 600);
let ast =
  ref([
    Glyph('2'),
    Glyph('x'),
    Glyph('+'),
    Glyph('5'),
    Glyph('='),
    Glyph('1'),
    Glyph('0'),
  ]);

let cursor = ref(List.length(ast^));

let rec remove_at = (n: int, l: list('a)) =>
  switch(l) {
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
         List.iteri(
           (i, child) => {
             if (cursor^ == i) {
               ctx
               |> Canvas2d.fillRect(
                    ~x=pen.x,
                    ~y=pen.y -. 0.85 *. 60.,
                    ~w=2.,
                    ~h=60.,
                  );
             };
             switch (child) {
             | Glyph(c) =>
               ctx
               |> Canvas2d.fillText(String.make(1, c), ~x=pen.x, ~y=pen.y);
               pen.x = pen.x +. metrics.getCharWidth(c, 60.);
             | _ => ignore()
             };
           },
           ast^,
         );

         if (cursor^ == List.length(ast^)) {
           ctx
           |> Canvas2d.fillRect(
                ~x=pen.x,
                ~y=pen.y -. 0.85 *. 60.,
                ~w=2.,
                ~h=60.,
              );
         };
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
               ast := remove_at(cursor^ - 1, ast^)
               cursor := cursor^ - 1;
             }
           | "ArrowLeft" => cursor := Js_math.max_int(0, cursor^ - 1)
           | "ArrowRight" =>
             cursor := Js_math.min_int(List.length(ast^), cursor^ + 1)
           | _ =>
             ast := insert_at(cursor^ - 1, Glyph(String.get(key, 0)), ast^);
             cursor := cursor^ + 1;
           };

           update();
         },
         document,
       );

       resolve();
     })
);