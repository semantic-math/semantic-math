[%%debugger.chrome];
Js.log("NewEditor");

open Webapi.Canvas;
/* open UniqueId; */
/* open EditorNode; */
/* open EditorTypsetter; */
open EditorRenderer;
/* open Cursor; */
open Tree;

exception Unhandled;

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

let rec insert_many_at = (n: int, xs: list('a), l: list('a)) =>
  switch (l) {
  | [] => n == 0 ? xs : []
  | [h, ...t] =>
    n == 0 ? xs @ [h, ...t] : [h, ...insert_many_at(n - 1, xs, t)]
  };

/* let insertIntoTree = (ast, path, index, newNode) => {
  let newAst =
    switch (nodeForPath(path, ast)) {
    | Some(pathNode) =>
      transform(
        node =>
          node == pathNode ?
            switch (pathNode) {
            | Box(id, kind, children) =>
              Some(Box(id, kind, insert_at(index, newNode, children)))
            | Glyph(_, _) => raise(Unhandled)
            } :
            Some(node),
        ast,
      )
    | None => raise(Unhandled)
    };
  switch (newAst) {
  | Some(node) => node
  | _ => raise(Unhandled)
  };
};

let insertManyIntoTree = (ast, path, index, newNodes) => {
  let newAst =
    switch (nodeForPath(path, ast)) {
    | Some(pathNode) =>
      transform(
        node =>
          node == pathNode ?
            switch (pathNode) {
            | Box(id, kind, children) =>
              Some(Box(id, kind, insert_many_at(index, newNodes, children)))
            | Glyph(_, _) => raise(Unhandled)
            } :
            Some(node),
        ast,
      )
    | None => raise(Unhandled)
    };
  switch (newAst) {
  | Some(node) => node
  | _ => raise(Unhandled)
  };
};

let removeNode = (nodeToRemove, ast) => {
  let newAst =
    transform(node => node == nodeToRemove ? None : Some(node), ast);
  switch (newAst) {
  | Some(node) => node
  | _ => raise(Unhandled)
  };
}; */

exception NoNodeForPath;

type cursor_path = list(int);

let processEvent = (key: string, cursor: ref(tree_cursor), ast: tree_node) =>
  switch (key) {
  | "Meta"
  | "Shift"
  | "Alt"
  | "Control" => ignore()
  | "Backspace" => ignore()
  | "ArrowLeft" =>
    cursor :=
      (
        switch (cursor^.prev) {
        | Some(node) =>
          switch (node.value) {
          | (_, Glyph(_)) => {
              prev: node.prev,
              next: Some(node),
              parent: cursor^.parent,
            }
          | (_, Frac({den})) =>
            switch (den.value) {
            | (_, Row({children})) => {
                prev: children.tail,
                next: None,
                parent: den,
              }
            | _ => raise(Invalid_Tree)
            }
          | (_, Parens({children})) => {
              prev: children.tail,
              next: None,
              parent: node,
            }
          | (_, SupSub({sub: Some(sub)})) => {
              prev: sub.tail,
              next: None,
              parent: node,
            }
          | (_, SupSub({sup: Some(sup)})) => {
              prev: sup.tail,
              next: None,
              parent: node,
            }
          | (_, Row({children})) => {
              prev: children.tail,
              next: None,
              parent: node,
            }
          | (_, SupSub({sub: None, sup: None})) => raise(Invalid_Node)
          }
        | None =>
          let node = cursor^.parent;
          switch (node.parent) {
          | Some(parent) =>
            switch (parent.value) {
            | (_, Frac({num, den})) =>
              if (node == den) {
                switch (num.value) {
                | (_, Row({children})) => {
                    prev: children.tail,
                    next: None,
                    parent: num,
                  }
                | _ => raise(Invalid_Tree)
                };
              } else {
                switch (parent.parent) {
                | Some(grandparent) => {
                    prev: parent.prev,
                    next: Some(parent),
                    parent: grandparent,
                  }
                | _ => raise(Invalid_Tree)
                };
              }
            | _ => {prev: node.prev, next: Some(node), parent}
            }
          | _ => cursor^ /* start of tree */
          };
        }
      )
  | "ArrowRight" =>
    cursor :=
      (
        switch (cursor^.next) {
        | Some(node) =>
          switch (node.value) {
          | (_, Glyph(_)) => {
              prev: Some(node),
              next: node.next,
              parent: cursor^.parent,
            }
          | (_, Frac({num})) =>
            switch (num.value) {
            | (_, Row({children})) => {
                prev: None,
                next: children.head,
                parent: num,
              }
            | _ => raise(Invalid_Tree)
            }
          | (_, Parens({children})) => {
              prev: None,
              next: children.head,
              parent: node,
            }
          | (_, SupSub({sub: Some(sub)})) => {
              prev: None,
              next: sub.head,
              parent: node,
            }
          | (_, SupSub({sup: Some(sup)})) => {
              prev: None,
              next: sup.head,
              parent: node,
            }
          | (_, Row({children})) => {
              prev: None,
              next: children.head,
              parent: node,
            }
          | (_, SupSub({sub: None, sup: None})) => raise(Invalid_Node)
          }
        | None =>
          let node = cursor^.parent;
          switch (node.parent) {
          | Some(parent) =>
            switch (parent.value) {
            | (_, Frac({num, den})) =>
              if (node == num) {
                switch (den.value) {
                | (_, Row({children})) => {
                    prev: None,
                    next: children.head,
                    parent: den,
                  }
                | _ => raise(Invalid_Tree)
                };
              } else {
                switch (parent.parent) {
                | Some(grandparent) => {
                    prev: Some(parent),
                    next: parent.next,
                    parent: grandparent,
                  }
                | _ => raise(Invalid_Tree)
                };
              }
            | _ => {prev: Some(node), next: node.next, parent}
            }
          | _ => cursor^ /* end of tree */
          };
        }
      )
  | "^" => ignore()
  | "_" => ignore()
  | "/" => ignore()
  | "(" => ignore()
  | _ =>
    let c = key.[0];
    ignore();
  };

let ctx = CanvasRenderer.makeContext(1600, 600);
let cursor = ref(Tree.cursor);

Js.Promise.(
  Fetch.fetch("/packages/typesetter/metrics/comic-sans.json")
  |> then_(Fetch.Response.json)
  |> then_(json => {
       let metrics = Metrics.make(json);

       let treeTypesetter = TreeTypesetter.makeTypesetter(metrics);
       let treeLayout = treeTypesetter.typeset(Tree.tree);

       let update = () => {
         /* clear canvas */
         ctx->Canvas2d.setFillStyle(String, "#FFFFFF");
         ctx |> Canvas2d.fillRect(~x=0., ~y=0., ~w=1600., ~h=600.);

         /* set styles */
         ctx->Canvas2d.setFillStyle(String, "#000000");

         Canvas2dRe.save(ctx);
         Canvas2dRe.translate(~x=100., ~y=300., ctx);
         renderLayout(ctx, treeLayout, cursor^);
         Canvas2dRe.restore(ctx);

         Js.log(Tree.tree);
       };

       update();
       open Webapi.Dom;

       Document.addKeyDownEventListener(
         event => {
           let key = KeyboardEvent.key(event);
           processEvent(key, cursor, Tree.tree);
           update();
         },
         document,
       );

       resolve();
     })
);