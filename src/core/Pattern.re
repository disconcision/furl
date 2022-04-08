open Sexplib.Std;

[@deriving sexp]
type atom =
  | Lit(int)
  | Var(string, list(Path.t))
  | Formless(string);

[@deriving sexp]
type t =
  | Atom(atom)
  //| Cons(name, list(atom))
  | Unknown(Word.s);

[@deriving sexp]
type uses_ctx = Environment.t_(list(Path.t));

let parse_atom: (uses_ctx, Word.t) => atom =
  (uses_map, {name, _}) =>
    switch (
      int_of_string_opt(name),
      Name.is_valid_var(name), //TODO: this will choke on emojis
      Environment.lookup(uses_map, name),
    ) {
    | (Some(n), _, _) => Lit(n)
    | (_, true, Some(uses)) => Var(name, uses)
    | (_, true, None) => Var(name, [])
    | _ => Formless(name)
    };

let parse: (uses_ctx, Word.s) => t =
  (ctx, words) => {
    switch (words) {
    | [x] => Atom(parse_atom(ctx, x))
    | _ => Unknown(words)
    };
  };

// idea: track variables that are used below but are unbound to suggest in pattern
