Js.log("NewEditor");

open Webapi.Canvas;
open Webapi.Dom;
open Metrics;

type point = {
  mutable x: float,
  mutable y: float,
};

let ctx = CanvasRenderer.makeContext(1000, 600);
let msg = ref("Hello, world!");
let cursor = ref(String.length(msg^));

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

         /* typeset stuff */
         let pen = {x: 0., y: 100.};
         String.iteri(
           (i, c) => {
             if (cursor^ == i) {
               ctx
               |> Canvas2d.fillRect(
                    ~x=pen.x,
                    ~y=pen.y -. 0.85 *. 60.,
                    ~w=2.,
                    ~h=60.,
                  );
             };

             ctx |> Canvas2d.fillText(String.make(1, c), ~x=pen.x, ~y=pen.y);
             pen.x = pen.x +. metrics.getCharWidth(c, 60.);
           },
           msg^,
         );

         if (cursor^ == String.length(msg^)) {
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
               msg :=
                 String.sub(msg^, 0, cursor^ - 1)
                 ++ String.sub(msg^, cursor^, String.length(msg^) - cursor^);
               cursor := cursor^ - 1;
             }
           | "ArrowLeft" => cursor := Js_math.max_int(0, cursor^ - 1)
           | "ArrowRight" =>
             cursor := Js_math.min_int(String.length(msg^), cursor^ + 1)
           | _ =>
             msg :=
               String.sub(msg^, 0, cursor^)
               ++ key
               ++ String.sub(msg^, cursor^, String.length(msg^) - cursor^);
             cursor := cursor^ + 1;
           };

           update();
         },
         document,
       );

       resolve();
     })
);