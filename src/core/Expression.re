open Sexplib.Std;

[@deriving sexp]
type prim =
  | Not
  | And
  | Or
  | Add
  | Mult
  | Fact
  | Equal
  | LessThan
  | MoreThan;

[@deriving sexp]
type operator =
  | AndOp
  | OrOp
  | Times
  | Plus
  | Minus
  | EqualOp
  | LessThanOp
  | MoreThanOp;

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

let prims = [
  "not",
  "and",
  "or",
  "sum",
  "prod",
  "fact",
  "desc",
  "asc",
  "equal",
];

let prim_of_string: string => option(prim) =
  fun
  | "not" => Some(Not)
  | "and" => Some(And)
  | "or" => Some(Or)
  | "sum" => Some(Add)
  | "prod" => Some(Mult)
  | "fact" => Some(Fact)
  | "equal" => Some(Equal)
  | "desc" => Some(MoreThan)
  | "asc" => Some(LessThan)
  | _ => None;

let prim_of: operator => option(prim) =
  fun
  | AndOp => Some(And)
  | OrOp => Some(Or)
  | Times => Some(Mult)
  | Plus => Some(Add)
  | EqualOp => Some(Equal)
  | LessThanOp => Some(LessThan)
  | MoreThanOp => Some(MoreThan)
  | Minus => None;

let parse_operator: string => option(operator) =
  fun
  | "&" => Some(AndOp)
  | "|" => Some(OrOp)
  | "*" => Some(Times)
  | "+" => Some(Plus)
  | "-" => Some(Minus)
  | "=" => Some(EqualOp)
  | "<" => Some(LessThanOp)
  | ">" => Some(MoreThanOp)
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

type parse_tail_res =
  | Failure
  | Any
  | Success(prim, list(t));

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
        switch (parse_tail_seq(xs, parse_word, ctx)) {
        | Success(op, ps) => Seq(op, [parse_word(x), ...ps])
        | Any
        | Failure => Unknown(words)
        }
      }
    };
  }
and parse_tail_seq = (xs, parse_word, ctx): parse_tail_res =>
  switch (xs) {
  | [] => Any
  | [op, x1, ...xs] =>
    switch (parse_operator(op), parse_tail_seq(xs, parse_word, ctx)) {
    | (Some(Minus), Success(Add, ps)) =>
      Success(Add, [parse_word("-" ++ x1), ...ps])
    | (Some(Minus), Any) => Success(Add, [parse_word("-" ++ x1)])
    | (Some(op), Success(prim, ps)) when prim_of(op) == Some(prim) =>
      Success(prim, [parse_word(x1), ...ps])
    | (Some(op), Any) when prim_of(op) != None =>
      switch (prim_of(op)) {
      | Some(prim) => Success(prim, [parse_word(x1)])
      | None => failwith("parse_tail_seq impossible")
      }
    | _ => Failure
    }
  | _ => Failure
  };
