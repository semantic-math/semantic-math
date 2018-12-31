open Jest;
open Expect;
open LinkedList;

module Int = {
  type t = int;
};

exception FooBar;

describe("LinkedList", () => {
  describe("push_tail", () =>
    test("should append to the end of a linked list", () => {
      let list = LinkedList.create();
      LinkedList.push_tail(1, list);
      LinkedList.push_tail(2, list);
      LinkedList.push_tail(3, list);

      expect(LinkedList.to_list(list)) |> toEqual([1, 2, 3]);
    })
  );

  describe("push_head", () =>
    test("should append to the end of a linked list", () => {
      let list = LinkedList.create();
      LinkedList.push_head(1, list);
      LinkedList.push_head(2, list);
      LinkedList.push_head(3, list);

      expect(LinkedList.to_list(list)) |> toEqual([3, 2, 1]);
    })
  );

  describe("insert_after", () => {
    test("should insert after the first item", () => {
      let list = LinkedList.create();
      LinkedList.push_tail(1, list);
      LinkedList.push_tail(2, list);
      LinkedList.push_tail(3, list);

      LinkedList.insert_after(0, 5, list);

      expect(LinkedList.to_list(list)) |> toEqual([1, 5, 2, 3]);
    });

    test("should insert after an item in the middle", () => {
      let list = LinkedList.create();
      LinkedList.push_tail(1, list);
      LinkedList.push_tail(2, list);
      LinkedList.push_tail(3, list);

      LinkedList.insert_after(1, 5, list);

      expect(LinkedList.to_list(list)) |> toEqual([1, 2, 5, 3]);
    });

    test("should insert after the last item", () => {
      let list = LinkedList.create();
      LinkedList.push_tail(1, list);
      LinkedList.push_tail(2, list);
      LinkedList.push_tail(3, list);

      LinkedList.insert_after(2, 5, list);

      expect(LinkedList.to_list(list)) |> toEqual([1, 2, 3, 5]);
    });

    test("should raise(Not_Found) if the index is past the end", () => {
      let list = LinkedList.create();
      LinkedList.push_tail(1, list);
      LinkedList.push_tail(2, list);
      LinkedList.push_tail(3, list);

      expect(() =>
        LinkedList.insert_after(3, 5, list)
      )
      |> toThrowException(LinkedList.Not_Found);
    });
  });
});
