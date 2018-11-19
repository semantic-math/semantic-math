let math = "1+x+x^2+x^3";
let tokens = Lexer.lex(math);
let ast = MathParser.parse(tokens);

Js.Promise.(
  Fetch.fetch("/packages/typesetter/metrics/comic-sans.json")
  |> then_(Fetch.Response.json)
  |> then_(json => {
       let metrics = Metrics.make(json);
       let typsetter = Typesetter.make(metrics);

       let renderToCanvas = math => {
         open Webapi.Canvas;
         open Canvas2d;

         let tokens = Lexer.lex(math);
         let ast = MathParser.parse(tokens);
         let box2 = Layout.hpackNat([typsetter.typeset(ast)]);
         Js.log(box2);

         /* TODO: replace with function to get full height */
         let {Layout.width, Layout.height, Layout.depth} = box2;
         let ctx =
           CanvasRenderer.makeContext(
             Js_math.ceil(width),
             Js_math.ceil(height +. depth),
           );

         /* enable retina mode */
         ctx |> scale(~x=2., ~y=2.);

         /* set styles */
         ctx->(setStrokeStyle(String, "magenta"));
         ctx->(setFillStyle(String, "#0000FF"));
         ctx->(lineWidth(1.));

         Canvas2dRe.save(ctx);
         Canvas2dRe.translate(~x=0., ~y=height, ctx);
         Renderer.render(ctx, box2, metrics);
         Canvas2dRe.restore(ctx);
       };

       renderToCanvas("e^-(x^2+y^2)");

       resolve();
     })
);
/* let layout = CanvasRenderer.layout; */

/* TODO: make sure font metrics are loaded */
/* CanvasRenderer.renderToCanvas("1+x+x^2+x^3"); */

Js.log("hello, world!");