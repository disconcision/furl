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

type uses_ctx = Environment.t_(list(Path.t));

let parse_atom: (uses_ctx, Word.t) => atom =
  (paths_map, word) =>
    switch (
      int_of_string_opt(word),
      Word.is_valid_var(word), //TODO: this will choke on emojis
      Environment.lookup(paths_map, word),
    ) {
    | (Some(n), _, _) => Lit(n)
    | (_, true, Some(uses)) => Var(word, uses)
    | _ => Formless(word)
    };

let parse: (uses_ctx, Word.s) => form =
  (ctx, words) =>
    switch (words) {
    | [x] => Atom(parse_atom(ctx, x))
    | _ => Unknown
    };

// idea: track variables that are used below but are unbound to suggest in pattern

/*
 plan for co-context:
 after first pass, all variables are now associated with their binding paths
 so technically we don't need to use names anymore
 we  could make a map of paths to lists of paths
 encounter variable use: append its path to entry for its binding path
 when we process a pattern, lookup its path

 alternatively if we used names, then we need to make sure to zero out
 the co-context after we encounter a pattern whose name is already in it


  */
