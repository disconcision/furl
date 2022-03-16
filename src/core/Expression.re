open Sexplib.Std;

[@deriving sexp]
type prim =
  | Not
  | Add
  | Mult
  | Fact;

[@deriving sexp]
type operator =
  | Times
  | Plus
  | Minus;

[@deriving sexp]
type t =
  | Atom(atom)
  | App(prim, list(t))
  | Seq(prim, list(t))
  | Unknown(Word.s)
  | Let(list(binding), t)
and binding = (Pattern.t, t)
and atom =
  | Lit(lit)
  | Var(string, Path.t)
  | Unbound(string)
  | Operator(string)
  | Formless(string)
and lit =
  | IntLit(int)
  | FloatLit(float)
  | BoolLit(bool)
  | Indet(t);

let string_of_lit: lit => string =
  fun
  | BoolLit(b) => string_of_bool(b)
  | IntLit(n) => string_of_int(n)
  | FloatLit(f) => string_of_float(f)
  | Indet(_) => "??";

let prim_of_string: string => option(prim) =
  fun
  | "sum" => Some(Add)
  | "prod" => Some(Mult)
  | "fact" => Some(Fact)
  | "not" => Some(Not)
  | _ => None;

let parse_operator: string => option(operator) =
  fun
  | "*" => Some(Times)
  | "+" => Some(Plus)
  | "-" => Some(Minus)
  | _ => None;

let is_operator: string => bool = s => parse_operator(s) != None;

let parse_lit: Word.t => option(lit) =
  word =>
    switch (
      bool_of_string_opt(word),
      int_of_string_opt(word),
      float_of_string_opt(word),
    ) {
    | (Some(b), _, _) => Some(BoolLit(b))
    | (_, Some(n), _) => Some(IntLit(n))
    | (_, _, Some(f)) => Some(FloatLit(f))
    | _ => None
    };

//TODO: combine with below
let parse_atom: (Path.ctx, Word.t) => atom =
  (context, word) =>
    switch (
      parse_lit(word),
      Word.is_valid_var(word),
      Environment.lookup(context, word),
    ) {
    | _ when is_operator(word) => Operator(word)
    | (Some(lit), _, _) => Lit(lit)
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

let rec parse: (Path.ctx, Word.s) => t =
  (ctx, words) => {
    let parse_word = word => Atom(parse_atom(ctx, word));
    switch (words) {
    | [] => Unknown(words)
    | [x] => parse_word(x)
    | [x, ...xs] =>
      switch (prim_of_string(x)) {
      | Some(fn) => App(fn, List.map(parse_word, xs))
      | _ =>
        switch (parse_tail_seq(words, xs, parse_word, ctx)) {
        | Some((op, ps)) => Seq(op, [parse_word(x), ...ps])
        | None => Unknown(words)
        }
      }
    };
  }
and parse_tail_seq = (words, xs, parse_word, ctx): option((prim, list('a))) =>
  switch (xs) {
  | [] => Some((Mult, [])) //TODO: this is not clear
  | [op, x1, ...xs] =>
    switch (parse_operator(op), parse_tail_seq(words, xs, parse_word, ctx)) {
    | (Some(Times), Some((Mult, ps)))
    | (Some(Times), Some((_, [] as ps))) =>
      Some((Mult, [parse_word(x1), ...ps]))
    | (Some(Plus), Some((Add, ps)))
    | (Some(Plus), Some((_, [] as ps))) =>
      Some((Add, [parse_word(x1), ...ps]))
    | (Some(Minus), Some((Add, ps)))
    | (Some(Minus), Some((_, [] as ps))) =>
      Some((Add, [parse_word("-" ++ x1), ...ps]))
    | _ => None
    }
  | _ => None
  };
