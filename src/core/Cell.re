open Sexplib.Std;

[@deriving sexp]
type uid = int;

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
  uid,
  pattern,
  expression,
  value,
};

[@deriving sexp]
type s = list(t);

[@deriving sexp]
type word_idx = int;

let nth_word: (Word.s, word_idx) => Word.t = List.nth;

let update_word: (string => string, int, Word.s) => Word.s =
  (f, idx) => List.mapi((i, x) => i == idx ? f(x) : x);

let cell_uid_gen = ref(0);

let mk_uid = (): int => {
  let uid = cell_uid_gen^;
  cell_uid_gen := cell_uid_gen^ + 1;
  uid;
};

let init_num: int => (Word.t, t) =
  n => {
    let w = Word.mk();
    let n = string_of_int(n);
    (w, {pattern: [w], expression: [n], value: [n], uid: mk_uid()});
  };

let init_name: Word.t => t =
  w => {
    {pattern: [w], expression: [Word.empty], value: ["?"], uid: mk_uid()};
  };

let init_full: ((Word.s, Word.s)) => t =
  ((pat, exp)) => {
    let _ = Word.mk();
    {pattern: pat, expression: exp, value: ["?"], uid: mk_uid()};
  };

let init: 'a => t = () => init_name(Word.mk());
