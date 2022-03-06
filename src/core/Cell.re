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

let init_num: int => (Word.t, t) =
  n => {
    let w = Word.mk();
    (
      w,
      {
        pattern: [w],
        expression: [string_of_int(n)],
        value: [string_of_int(n)],
      },
    );
  };

let init_name: Word.t => t =
  w => {
    {pattern: [w], expression: [Word.empty], value: ["?"]};
  };

let init: 'a => t = () => init_name(Word.mk());
