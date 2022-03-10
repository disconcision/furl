open Sexplib.Std;

[@deriving sexp]
type atom =
  | Lit(int)
  | Unknown(Word.t);

[@deriving sexp]
type t =
  | Atom(atom)
  | Unknown(Word.s);

let parse_atom: Word.t => atom =
  w =>
    switch (int_of_string_opt(w)) {
    | Some(n) => Lit(n)
    | None => Unknown(w)
    };

let parse: Word.s => t =
  fun
  | [w] => Atom(parse_atom(w))
  | ws => Unknown(ws);
