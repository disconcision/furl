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
  //id,
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

let init: 'a => t =
  () => {
    {pattern: [Word.mk()], expression: [Word.empty], value: ["?"]};
  };
