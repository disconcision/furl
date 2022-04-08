open Sexplib.Std;

[@deriving sexp]
type pattern = Word.s;
[@deriving sexp]
type expression = Word.s;
[@deriving sexp]
type value = Word.s;

[@deriving sexp]
type field =
  | Pattern
  | Expression
  | Value;

[@deriving sexp]
type t = {
  uid: UID.t,
  pattern,
  expression,
  value,
};

[@deriving sexp]
type s = list(t);

[@deriving sexp]
type word_idx = int;

let ids: ref(list(UID.t)) = ref([]);

let nth_word: (Word.s, word_idx) => Word.t = List.nth;

let update_word: (Name.t => Name.t, int, Word.s) => Word.s =
  (f, idx) =>
    List.mapi((i, {name, _} as w: Word.t) =>
      {...w, name: i == idx ? f(name) : name}
    );

let mk_uid = () => {
  let uid = UID.mk();
  ids := [uid, ...ids^];
  uid;
};

//TODO: betterize this (cyclical dep withe Expr issue)
let init_w': Word.t => (Word.t, t) =
  w_exp => {
    let w = Word.mk();
    (
      w,
      {pattern: [w], expression: [w_exp], value: [w_exp], uid: mk_uid()},
    );
  };

let init_w: Name.t => (Word.t, t) = w_exp => init_w'(Word.mk_name(w_exp));

let init_name': Word.t => t =
  w => {
    {
      pattern: [w],
      expression: [Word.mk_empty()],
      value: [Word.mk_name("?")],
      uid: mk_uid(),
    };
  };

let init_name: Name.t => t = n => init_name'(Word.mk_name(n));

let init_full: ((Name.s, Name.s)) => t =
  ((pat, exp)) => {
    let _ = Word.mk();
    {
      pattern: List.map(Word.mk_name, pat),
      expression: List.map(Word.mk_name, exp),
      value: [Word.mk_name("?")],
      uid: mk_uid(),
    };
  };

let init: 'a => t = () => init_name'(Word.mk());

let copy: t => t = cell => {...cell, uid: mk_uid()};
