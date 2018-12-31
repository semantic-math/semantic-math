exception Invalid_LinkedList;
exception Not_Found;
type node('a) = {
  mutable value: 'a,
  mutable prev: option(node('a)),
  mutable next: option(node('a)),
  mutable parent: option(node('a)),
}
and linked_list('a) = {
  mutable head: option(node('a)),
  mutable tail: option(node('a)),
};

let create = (): linked_list('a) => {head: None, tail: None};

let push_tail = (~parent=None, value: 'a, l: linked_list('a)) =>
  switch (l) {
  | {head: Some(_), tail: Some(tl)} =>
    tl.next = Some({value, prev: Some(tl), next: None, parent});
    l.tail = tl.next;
  | {head: None, tail: None} =>
    let node = {value, prev: None, next: None, parent};
    l.head = Some(node);
    l.tail = Some(node);
  | _ => raise(Invalid_LinkedList)
  };

let from_list = (~parent=None, l: list('a)): linked_list('a) =>
  List.fold_left(
    (ll, value) => {
      push_tail(~parent, value, ll);
      ll;
    },
    create(),
    l,
  );

let push_head = (~parent=None, value: 'a, l: linked_list('a)) =>
  switch (l) {
  | {head: Some(hd), tail: Some(_)} =>
    hd.prev = Some({value, prev: None, next: Some(hd), parent});
    l.head = hd.prev;
  | {head: None, tail: None} =>
    let node = {value, prev: None, next: None, parent};
    l.head = Some(node);
    l.tail = Some(node);
  | _ => raise(Invalid_LinkedList)
  };

let iteri = (f: (int, 'a) => unit, l: linked_list('a)) => {
  let rec next = (i: int, x: option(node('a))) => {
    switch (x) {
    | Some(node) => f(i, node.value); next(i + 1, node.next);
    | None => ();
    }
  };
  next(0, l.head);
};

/* let foo = (~bar=None, index: int) => (); */

let fold_lefti = (f: ('b, int, 'a) => 'b, accu: 'b, l: linked_list('a)): 'b => {
  let accu = ref(accu);
  iteri((i, x) => accu := f(accu^, i, x), l);
  accu^;
};

let iter = (f: 'a => unit, l: linked_list('a)) =>
  iteri((_, x) => f(x), l);

let iteri_nodes = (f: (int, node('a)) => unit, l: linked_list('a)) => {
  let rec next = (i: int, x: option(node('a))) => {
    switch (x) {
    | Some(node) => f(i, node); next(i + 1, node.next);
    | None => ();
    }
  };
  next(0, l.head);
};

let iter_nodes = (f: node('a) => unit, l: linked_list('a)) => 
  iteri_nodes((_, x) => f(x), l);

let fold_left = (f: ('b, 'a) => 'b, accu: 'b, l: linked_list('a)): 'b => 
  fold_lefti((accu, _, x) => f(accu, x), accu, l);

let to_list = (l: linked_list('a)): list('a) =>
  fold_left((accu, x) => accu @ [x], [], l);

let nth = (index: int, l: linked_list('a)) =>
  fold_lefti(
    (accu, i, x) =>
      switch (accu) {
      | None => i == index ? Some(x) : None
      | _ => accu
      },
    None,
    l,
  );

/* TODO: replace with a functional approach */
let nth_node = (index: int, l: linked_list('a)) => {
  if (index < 0) {
    raise(Invalid_argument("index"));
  };
  let node = ref(l.head);
  let i = ref(0);
  let result = ref(None);
  while (switch (node^) {
          | Some(x) =>
            if (index == i^) {
              result := Some(x);
            };
            node := x.next;
            i := i^ + 1;
            true;
          | None => false
          }) {
    ();
  };
  switch (result^) {
  | Some(node) => node
  | None => raise(Not_Found)
  };
};

let insert_after =
    (~parent=None, index: int, value: 'a, l: linked_list('a)) => {
  let node = nth_node(index, l);
  if (l.tail == Some(node)) {
    node.next = Some({value, prev: l.tail, next: None, parent});
    l.tail = node.next;
  } else {
    switch (node.next) {
    | Some(next) =>
      node.next =
        Some({value, prev: Some(node), next: Some(next), parent});
      next.prev = node.next;
    | _ => raise(Invalid_LinkedList)
    };
  };
};
