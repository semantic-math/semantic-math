open LinkedList;
open UniqueId;

exception Invalid_Tree;

type glyph = {
  char,
}
and row = {
  children: linked_list(tree_node),
}
and parens = {
  children: linked_list(tree_node),
}
and supsub = {
  sup: option(linked_list(tree_node)),
  sub: option(linked_list(tree_node)),
}
/* we could solve this issue with polymorphic types */
and frac = {
  num: node(tree_node),
  den: node(tree_node),
}
and tree_type =
  | Glyph(glyph)
  | Row(row)
  | Parens(parens)
  | Frac(frac)
  | SupSub(supsub)
and tree_node = (int, tree_type);

let children = LinkedList.create();
LinkedList.push_tail((genId(), Glyph({char: '2'})), children);
LinkedList.push_tail((genId(), Glyph({char: 'x'})), children);

let sup = LinkedList.from_list([(genId(), Glyph({char: '2'}))]);
LinkedList.push_tail(
  (genId(), SupSub({sup: Some(sup), sub: None})),
  children,
);
LinkedList.iter_nodes(child => child.parent = children.tail, sup);

LinkedList.push_tail((genId(), Glyph({char: '+'})), children);

let numChildren = LinkedList.from_list([(genId(), Glyph({char: '1'}))]);
let num = {
  value: (genId(), Row({children: numChildren})),
  prev: None,
  next: None,
  parent: None,
};

let denChildren =
  LinkedList.from_list([
    (genId(), Glyph({char: 'x'})),
    (genId(), Glyph({char: '+'})),
    (genId(), Glyph({char: '1'})),
  ]);
let den = {
  value: (genId(), Row({children: denChildren})),
  prev: None,
  next: None,
  parent: None,
};

num.next = Some(den);
den.prev = Some(num);

LinkedList.push_tail((genId(), Frac({num, den})), children);
LinkedList.iter_nodes(child => child.parent = Some(num), numChildren);
LinkedList.iter_nodes(child => child.parent = Some(den), denChildren);
num.parent = children.tail;
den.parent = children.tail;

LinkedList.push_tail((genId(), Glyph({char: '+'})), children);

let paren =
  LinkedList.from_list([
    (genId(), Glyph({char: '5'})),
    (genId(), Glyph({char: '-'})),
    (genId(), Glyph({char: 'x'})),
  ]);
LinkedList.push_tail((genId(), Parens({children: paren})), children);
LinkedList.iter_nodes(child => child.parent = children.tail, paren);

LinkedList.push_tail((genId(), Glyph({char: '='})), children);
LinkedList.push_tail((genId(), Glyph({char: '1'})), children);
LinkedList.push_tail((genId(), Glyph({char: '0'})), children);

let tree = (genId(), Row({children: children}));
let treeNode = {
    prev: None,
    next: None,
    parent: None,
    value: tree,
};
LinkedList.iter_nodes(child => child.parent = Some(treeNode), children);

type cursor('a) = {
  prev: option(LinkedList.node('a)),
  next: option(LinkedList.node('a)),
  parent: LinkedList.node('a),
};

type tree_cursor = cursor(tree_node);

let cursor = {
  prev: Some(LinkedList.nth_node(0, children)),
  next: Some(LinkedList.nth_node(1, children)),
  parent: {
    prev: None,
    next: None,
    parent: None,
    value: tree,
  },
};

exception Invalid_Node;

let moveLeft = (cursor: tree_cursor) =>
  switch (cursor.prev) {
  | Some(node) =>
    switch (node.value) {
    | (_, Glyph(_)) => {prev: node.prev, next: Some(node), parent: cursor.parent}
    | (_, Frac({den})) =>
      switch (den.value) {
      | (_, Row({children})) => {prev: children.tail, next: None, parent: den}
      | _ => raise(Invalid_Tree)
      }
    | (_, Parens({children})) => {prev: children.tail, next: None, parent: node}
    | (_, SupSub({sub: Some(sub)})) => {prev: sub.tail, next: None, parent: node}
    | (_, SupSub({sup: Some(sup)})) => {prev: sup.tail, next: None, parent: node}
    | (_, Row({children})) => {prev: children.tail, next: None, parent: node}
    | (_, SupSub({sub: None, sup: None})) => raise(Invalid_Node)
    }
  | None =>
    let node = cursor.parent;
    switch (node.parent) {
    | Some(parent) => {prev: node.prev, next: Some(node), parent}
    | _ => raise(Invalid_Tree)
    };
  };