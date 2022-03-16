[@deriving sexp]
type atom =
  | Lit(Expression.lit)
  | Unknown(Word.t);

[@deriving sexp]
type t =
  | Atom(atom)
  | Unknown(Word.s);

let parse_atom: Word.t => atom =
  w =>
    switch (Expression.parse_lit(w)) {
    | Some(n) => Lit(n)
    | None => Unknown(w)
    };

let parse: Word.s => t =
  fun
  | [w] => Atom(parse_atom(w))
  | ws => Unknown(ws);
