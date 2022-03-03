open Sexplib.Std;

[@deriving sexp]
type atom =
  | Lit(int)
  | Var(string, Path.t)
  | Unbound(string)
  | Formless(string);

[@deriving sexp]
type prim =
  | Add
  | Mult
  | Fact;

[@deriving sexp]
type form =
  | Atom(atom)
  | App(prim, list(form))
  | Seq(prim, list(form))
  | Unknown;

let prim_of_string: string => option(prim) =
  fun
  | "add" => Some(Add)
  | "mult" => Some(Mult)
  | "fact" => Some(Fact)
  | _ => None;

//TODO: combine with below
let parse_atom: (Path.ctx, Word.t) => atom =
  (context, word) =>
    switch (
      int_of_string_opt(word),
      Word.is_valid_var(word),
      Environment.lookup(context, word),
    ) {
    | (Some(n), _, _) => Lit(n)
    | (_, true, Some(path)) => Var(word, path)
    | (_, true, None) => Unbound(word)
    | _ => Formless(word)
    };

let is_bound_var: atom => bool =
  form =>
    switch (form) {
    | Var(_) => true
    | _ => false
    };

let parse: (Path.ctx, Word.s) => form =
  (ctx, words) => {
    let parse_word = word => Atom(parse_atom(ctx, word));
    switch (words) {
    | [] => Unknown
    | [x] => parse_word(x)
    | [x, ...xs] =>
      switch (prim_of_string(x)) {
      | Some(fn) => App(fn, List.map(parse_word, xs))
      | _ =>
        switch (xs) {
        | ["+", x1] => Seq(Add, List.map(parse_word, [x, x1]))
        | ["+", x1, "+", x2] => Seq(Add, List.map(parse_word, [x, x1, x2]))
        | ["+", x1, "+", x2, "+", x3] =>
          Seq(Add, List.map(parse_word, [x, x1, x2, x3]))
        | ["*", x1] => Seq(Mult, List.map(parse_word, [x, x1]))
        | ["*", x1, "*", x2] =>
          Seq(Mult, List.map(parse_word, [x, x1, x2]))
        | ["*", x1, "*", x2, "*", x3] =>
          Seq(Mult, List.map(parse_word, [x, x1, x2, x3]))
        | ["-", x1] => Seq(Add, List.map(parse_word, [x, "-" ++ x1]))
        | ["-", x1, "-", x2] =>
          Seq(Add, List.map(parse_word, [x, "-" ++ x1, "-" ++ x2]))
        | _ => Unknown
        }
      }
    };
  };
