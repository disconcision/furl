open Sexplib.Std;

[@deriving sexp]
type atom =
  | Lit(int)
  | Var(string)
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

let valid_var_regex =
  Re.Str.regexp("^\\([a-zA-Z]\\|_[_a-zA-Z0-9]\\)[_a-zA-Z0-9']*$");
let is_valid_var = s => Re.Str.string_match(valid_var_regex, s, 0);

//TODO: combine with below
let parse_exp_atom: (Path.ctx, Word.t) => atom =
  (context, word) =>
    switch (
      int_of_string_opt(word),
      is_valid_var(word),
      Environment.lookup(context, word),
    ) {
    | (Some(n), _, _) => Lit(n)
    | (_, true, Some(_)) => Var(word)
    | (_, true, None) => Unbound(word)
    | _ => Formless(word)
    };

let is_bound_var: atom => bool =
  form =>
    switch (form) {
    | Var(_) => true
    | _ => false
    };

//TODO: combine with above
let parse_atom': Word.t => atom =
  fun
  | n when int_of_string_opt(n) != None => Lit(int_of_string(n))
  | v => Var(v);
let parse_atom: Word.t => form = word => Atom(parse_atom'(word));

let parse: Word.s => form =
  fun
  | [] => Unknown
  | [x] => parse_atom(x)
  | [x, ...xs] =>
    switch (prim_of_string(x)) {
    | Some(fn) => App(fn, List.map(parse_atom, xs))
    | _ =>
      switch (xs) {
      | ["+", x1] => Seq(Add, List.map(parse_atom, [x, x1]))
      | ["+", x1, "+", x2] => Seq(Add, List.map(parse_atom, [x, x1, x2]))
      | ["+", x1, "+", x2, "+", x3] =>
        Seq(Add, List.map(parse_atom, [x, x1, x2, x3]))
      | ["*", x1] => Seq(Mult, List.map(parse_atom, [x, x1]))
      | ["*", x1, "*", x2] => Seq(Mult, List.map(parse_atom, [x, x1, x2]))
      | ["*", x1, "*", x2, "*", x3] =>
        Seq(Mult, List.map(parse_atom, [x, x1, x2, x3]))
      | ["-", x1] => Seq(Add, List.map(parse_atom, [x, "-" ++ x1]))
      | ["-", x1, "-", x2] =>
        Seq(Add, List.map(parse_atom, [x, "-" ++ x1, "-" ++ x2]))
      | _ => Unknown
      }
    };
