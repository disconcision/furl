open Sexplib.Std;

let rec range = (~lo=0, hi) =>
  if (lo > hi) {
    raise(Invalid_argument("ListUtil.range"));
  } else if (lo == hi) {
    [];
  } else {
    [lo, ...range(~lo=lo + 1, hi)];
  };

// heads of prefix and suffix neighbor the subject
[@deriving sexp]
type frame('x) = (list('x), list('x));

let rec mk_frame = (n: int, xs: list('x)): frame('x) => {
  let invalid_arg = () => raise(Invalid_argument("ListUtil.mk_frame"));
  if (n < 0) {
    invalid_arg();
  } else if (n == 0) {
    ([], xs);
  } else {
    switch (xs) {
    | [] => invalid_arg()
    | [x, ...xs] =>
      let (prefix, suffix) = mk_frame(n - 1, xs);
      (prefix @ [x], suffix);
    };
  };
};

let rec split_frame = (n: int, xs: list('x)): ('x, frame('x)) =>
  switch (n, xs) {
  | (_, []) => failwith("list index out of bounds")
  | (0, [x, ...xs]) => (x, ([], xs))
  | (_, [x, ...xs]) =>
    let (subj, (prefix, suffix)) = split_frame(n - 1, xs);
    (subj, (prefix @ [x], suffix));
  };

let of_frame = (~subject: list('x)=[], (prefix, suffix): frame('x)) =>
  List.concat([List.rev(prefix), subject, suffix]);

let combine_opt = (xs, ys) =>
  switch (List.combine(xs, ys)) {
  | exception (Invalid_argument(_)) => None
  | xys => Some(xys)
  };

let is_empty =
  fun
  | [] => true
  | _ => false;

let flat_map = (f, l) => List.flatten(List.map(f, l));

let rec join = (sep: 'x, xs: list('x)): list('x) =>
  switch (xs) {
  | [] => []
  | [x] => [x]
  | [x, ...xs] => [x, sep, ...join(sep, xs)]
  };

let hd_opt =
  fun
  | [] => None
  | [hd, ..._] => Some(hd);

let rec nth_opt = (n, xs) =>
  n < 0
    ? None
    : (
      switch (xs) {
      | [] => None
      | [hd, ...tl] => n == 0 ? Some(hd) : nth_opt(n - 1, tl)
      }
    );

/**
 * `split_n_opt(n, xs)` splits the first `n` elements from `xs`
 * if `xs` has `n` or more elements
 */
let split_n_opt = (n: int, xs: list('x)): option((list('x), list('x))) => {
  let rec go = (n: int, xs: list('x)) =>
    if (n < 0) {
      None;
    } else if (n == 0) {
      Some(([], xs));
    } else {
      switch (xs) {
      | [] => None
      | [x, ...xs] =>
        go(n - 1, xs)
        |> Option.map(((prefix, suffix)) => ([x, ...prefix], suffix))
      };
    };
  go(n, xs);
};

let split_n = (n: int, xs: list('x)): (list('x), list('x)) =>
  switch (split_n_opt(n, xs)) {
  | None =>
    raise(Invalid_argument("ListUtil.split_n: " ++ string_of_int(n)))
  | Some(r) => r
  };

/**
 * Returns sublist from index i (inclusive)
 * to index j (exclusive), coupled with the
 * surrounding prefix/suffix sublists.
 * Returns None if i > j.
 */
let split_sublist_opt =
    (i: int, j: int, xs: list('x))
    : option((list('x), list('x), list('x))) => {
  switch (split_n_opt(j, xs)) {
  | None => None
  | Some((left, right)) =>
    switch (split_n_opt(i, left)) {
    | None => None
    | Some((left, mid)) => Some((left, mid, right))
    }
  };
};
let split_sublist =
    (i: int, j: int, xs: list('x)): (list('x), list('x), list('x)) =>
  switch (split_sublist_opt(i, j, xs)) {
  | None => raise(Invalid_argument("ListUtil.split_sublist"))
  | Some(r) => r
  };
let sublist = ((i, j), xs: list('x)): list('x) => {
  let (_, sublist, _) = split_sublist(i, j, xs);
  sublist;
};

let rec split_nth_opt = (n, xs) =>
  switch (n, xs) {
  | _ when n < 0 => None
  | (_, []) => None
  | (0, [x, ...suffix]) => Some(([], x, suffix))
  | (_, [x, ...xs]) =>
    split_nth_opt(n - 1, xs)
    |> Option.map(((prefix, subject, suffix)) =>
         ([x, ...prefix], subject, suffix)
       )
  };
let split_nth = (n, xs) =>
  switch (split_nth_opt(n, xs)) {
  | None =>
    raise(Invalid_argument("ListUtil.split_nth: " ++ string_of_int(n)))
  | Some(r) => r
  };

let rec put_nth = (n: int, x: 'x, xs: list('x)): list('x) =>
  switch (n, xs) {
  | (_, []) => failwith("out of bounds")
  | (0, [_, ...tl]) => [x, ...tl]
  | (_, [hd, ...tl]) =>
    let tl = put_nth(n - 1, x, tl);
    [hd, ...tl];
  };

let rec split_last_opt = (xs: list('x)): option((list('x), 'x)) =>
  switch (xs) {
  | [] => None
  | [x] => Some(([], x))
  | [x, ...xs] =>
    split_last_opt(xs)
    |> Option.map(((leading, last)) => ([x, ...leading], last))
  };
let last_opt = xs => xs |> split_last_opt |> Option.map(snd);

let split_last = (xs: list('x)): (list('x), 'x) =>
  switch (split_last_opt(xs)) {
  | None => raise(Invalid_argument("ListUtil.split_last"))
  | Some(r) => r
  };
let leading = xs => fst(split_last(xs));

let split_first = (xs: list('x)): ('x, list('x)) =>
  switch (xs) {
  | [] => raise(Invalid_argument("ListUtil.split_first"))
  | [first, ...trailing] => (first, trailing)
  };

let rec fold_left_map =
        (f: ('acc, 'x) => ('acc, 'y), start: 'acc, xs: list('x))
        : ('acc, list('y)) =>
  switch (xs) {
  | [] => (start, [])
  | [x, ...xs] =>
    let (new_acc, y) = f(start, x);
    let (final, ys) = fold_left_map(f, new_acc, xs);
    (final, [y, ...ys]);
  };

let rec take_while = (p: 'x => bool, xs: list('x)): (list('x), list('x)) =>
  switch (xs) {
  | [] => ([], [])
  | [hd, ...tl] =>
    if (p(hd)) {
      let (taken, rest) = take_while(p, tl);
      ([hd, ...taken], rest);
    } else {
      ([], xs);
    }
  };

let take_2 =
  fun
  | [x1, x2, ..._] => (x1, x2)
  | _ => raise(Invalid_argument("ListUtil.take_2"));
let take_3 =
  fun
  | [x1, x2, x3, ..._] => (x1, x2, x3)
  | _ => raise(Invalid_argument("ListUtil.take_3"));
let take_4 =
  fun
  | [x1, x2, x3, x4, ..._] => (x1, x2, x3, x4)
  | _ => raise(Invalid_argument("ListUtil.take_4"));
let take_5 =
  fun
  | [x1, x2, x3, x4, x5, ..._] => (x1, x2, x3, x4, x5)
  | _ => raise(Invalid_argument("ListUtil.take_5"));

// new:

let rec intersperse: ('a, list('a)) => list('a) =
  (sep, ls) =>
    switch (ls) {
    | []
    | [_] => ls
    | [hd, ...tl] => [hd] @ [sep] @ intersperse(sep, tl)
    };

let rec interleave: (list('a), list('a)) => list('a) =
  (xs, ys) =>
    switch (xs) {
    | [] => ys
    | [x, ...xs] => [x, ...interleave(ys, xs)]
    };

let swap: (int, int, list('a)) => list('a) =
  (u, v, xs) => {
    let e_u = List.nth(xs, u);
    let e_v = List.nth(xs, v);
    List.mapi((i, x) => i == u ? e_v : i == v ? e_u : x, xs);
  };

let insert_at: (int, 'a, list('a)) => list('a) =
  (i, n, xs) =>
    if (i == List.length(xs)) {
      xs @ [n];
    } else {
      List.fold_left2(
        (acc, x, idx) => {acc @ (i == idx ? [n, x] : [x])},
        [],
        xs,
        List.init(List.length(xs), x => x),
      );
    };

let remove: (int, list('a)) => list('a) =
  (i, xs) =>
    List.fold_left2(
      (acc, x, idx) => {acc @ (i == idx ? [] : [x])},
      [],
      xs,
      List.init(List.length(xs), x => x),
    );
