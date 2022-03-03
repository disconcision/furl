open Sexplib.Std;

[@deriving sexp]
type atom =
  | Lit(int)
  | Var(string, list(Path.t))
  | Formless(string);

[@deriving sexp]
type form =
  | Atom(atom)
  //| Cons(name, list(atom))
  | Unknown;

[@deriving sexp]
type uses_ctx = Environment.t_(list(Path.t));

let parse_atom: (uses_ctx, Word.t) => atom =
  (uses_map, word) =>
    switch (
      int_of_string_opt(word),
      Word.is_valid_var(word), //TODO: this will choke on emojis
      Environment.lookup(uses_map, word),
    ) {
    | (Some(n), _, _) => Lit(n)
    | (_, true, Some(uses)) => Var(word, uses)
    | (_, true, None) => Var(word, [])
    | _ => Formless(word)
    };

let parse: (uses_ctx, Word.s) => form =
  (ctx, words) =>
    switch (words) {
    | [x] => Atom(parse_atom(ctx, x))
    | _ => Unknown
    };

// idea: track variables that are used below but are unbound to suggest in pattern
