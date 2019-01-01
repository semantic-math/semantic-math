[%%debugger.chrome];
Js.log("NewEditor");

open Webapi.Canvas;
open UniqueId;
/* open EditorNode; */
/* open EditorTypsetter; */
open EditorRenderer;
/* open Cursor; */
open Tree;

exception Unhandled;
exception NoNodeForPath;
exception No_Children;

type cursor_path = list(int);

let rec firstChild = (tn: tree_node): option(LinkedList.node(tree_node)) =>
  switch (tn) {
  | (_, Frac({num})) => firstChild(num.value)
  | (_, Parens({children})) => children.head
  | (_, SupSub({sub: Some(sub)})) => sub.head
  | (_, SupSub({sup: Some(sup)})) => sup.head
  | (_, Row({children})) => children.head
  | _ => raise(No_Children)
  };

let rec lastChild = (tn: tree_node): option(LinkedList.node(tree_node)) =>
  switch (tn) {
  | (_, Frac({den})) => lastChild(den.value)
  | (_, Parens({children})) => children.tail
  | (_, SupSub({sub: Some(sub)})) => sub.tail
  | (_, SupSub({sup: Some(sup)})) => sup.tail
  | (_, Row({children})) => children.tail
  | _ => raise(No_Children)
  };

let processEvent = (key: string, cursor: ref(tree_cursor), ast: tree_node) =>
  switch (key) {
  | "Meta"
  | "Shift"
  | "Alt"
  | "Control" => ignore()
  | "Backspace" =>
    let parent = cursor^.parent;
    switch (cursor^.prev) {
    | Some(node) =>
      switch (parent) {
      | {value: (_, Row({children}))}
      | {value: (_, Parens({children}))} =>
        LinkedList.remove_node(node, children)
      | _ => ()
      }
    | _ => ()
    };
    let prev =
      switch (cursor^.prev) {
      | Some(node) => node.prev
      | None => None
      };
    cursor := {next: cursor^.next, prev, parent: cursor^.parent};
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
          | _ => {prev: lastChild(node.value), next: None, parent: node}
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
          | _ => {prev: None, next: firstChild(node.value), parent: node}
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
    let glyph = (genId(), Glyph({char: c}));
    let parent = cursor^.parent;
    switch (parent) {
    | {value: (_, Row({children}))}
    | {value: (_, Parens({children}))} =>
      switch (cursor^.prev) {
      | Some(prev) =>
        LinkedList.insert_after_node(
          ~parent=Some(parent),
          prev,
          glyph,
          children,
        )
      | _ => LinkedList.push_head(~parent=Some(parent), glyph, children)
      };
      cursor :=
        (
          switch (cursor^) {
          | {next: Some(node)} => {
              prev: node.prev,
              next: Some(node),
              parent: cursor^.parent,
            }
          | {prev: Some(node)} => {
              prev: node.next,
              next: None,
              parent: cursor^.parent,
            }
          | _ => raise(Unhandled) /* TODO: handle this case */
          }
        );
    | _ => ()
    };
  };

let ctx = CanvasRenderer.makeContext(1600, 600);
let cursor = ref(Tree.cursor);

Js.Promise.(
  Fetch.fetch("/packages/typesetter/metrics/comic-sans.json")
  |> then_(Fetch.Response.json)
  |> then_(json => {
       let metrics = Metrics.make(json);

       let treeTypesetter = TreeTypesetter.makeTypesetter(metrics);

       let update = () => {
         let treeLayout = treeTypesetter.typeset(Tree.tree);

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
