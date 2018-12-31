open LinkedList;

exception Invalid_Tree;

type glyph = {char}
and row = {children: linked_list(tree_node)}
and parens = {children: linked_list(tree_node)}
and supsub = {
  sup: option(linked_list(tree_node)),
  sub: option(linked_list(tree_node)),
}
and frac = {
  num: node(tree_node),
  den: node(tree_node),
}
and tree_node =
  | Glyph(glyph)
  | Row(row)
  | Parens(parens)
  | Frac(frac)
  | SupSub(supsub);

let children = LinkedList.create();
LinkedList.push_tail(Glyph({char: '2'}), children);
LinkedList.push_tail(Glyph({char: 'x'}), children);

let sup = LinkedList.from_list([Glyph({char: '2'})]);
LinkedList.push_tail(SupSub({sup: Some(sup), sub: None}), children);
LinkedList.iter_nodes(child => child.parent = children.tail, sup);

LinkedList.push_tail(Glyph({char: '+'}), children);

let numChildren = LinkedList.from_list([Glyph({char: '1'})]);
let num = {
  value: Row({children: numChildren}),
  prev: None,
  next: None,
  parent: None,
};

let denChildren =
  LinkedList.from_list([
    Glyph({char: 'x'}),
    Glyph({char: '+'}),
    Glyph({char: '1'}),
  ]);
let den = {
  value: Row({children: denChildren}),
  prev: None,
  next: None,
  parent: None,
};

LinkedList.push_tail(Frac({num, den}), children);
LinkedList.iter_nodes(child => child.parent = children.tail, numChildren);
LinkedList.iter_nodes(child => child.parent = children.tail, denChildren);

LinkedList.push_tail(Glyph({char: '+'}), children);

let paren =
  LinkedList.from_list([
    Glyph({char: '5'}),
    Glyph({char: '-'}),
    Glyph({char: 'x'}),
  ]);
LinkedList.push_tail(Parens({children: paren}), children);
LinkedList.iter_nodes(child => child.parent = children.tail, paren);

LinkedList.push_tail(Glyph({char: '='}), children);
LinkedList.push_tail(Glyph({char: '1'}), children);
LinkedList.push_tail(Glyph({char: '0'}), children);

let tree = Row({children: children});

type cursor('a) = {
  prev: option(LinkedList.node('a)),
  next: option(LinkedList.node('a)),
  parent: option(LinkedList.node('a)),
};

exception Invalid_Node;

let moveLeft = (cursor: cursor(tree_node)) =>
  switch (cursor.prev) {
  | Some(node) =>
    switch (node.value) {
    | Glyph(_) => {prev: node.prev, next: Some(node), parent: cursor.parent}
    | Frac({den}) =>
      switch (den.value) {
      | Row({children}) => {
          prev: children.tail,
          next: None,
          parent: Some(den),
        }
      | _ => raise(Invalid_Tree)
      }
    | Parens({children}) => {
        prev: children.tail,
        next: None,
        parent: Some(node),
      }
    | SupSub({sub: Some(sub)}) => {
        prev: sub.tail,
        next: None,
        parent: Some(node),
      }
    | SupSub({sup: Some(sup)}) => {
        prev: sup.tail,
        next: None,
        parent: Some(node),
      }
    | Row({children}) => {
        prev: children.tail,
        next: None,
        parent: Some(node),
      }
    | SupSub({sub: None, sup: None}) => raise(Invalid_Node)
    }
  | None =>
    switch (cursor.parent) {
    | Some(node) => {prev: node.prev, next: Some(node), parent: node.parent}
    | None => cursor
    }
  };