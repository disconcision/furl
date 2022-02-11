// interpreter
// to start out with:
// integer literals
// prebuilt constant variables
// referring to integers and prebuilt functions
// application of prebuilt functions
// application of infix operator(s); initially just +
/* variable bindings*/

type env = Environment.t_(option(int));

type prim_fn =
  | Add
  | Mult
  | Fact;

type form =
  | Lit(int)
  | Var(string)
  | App(prim_fn, list(form))
  | Unknown;

let prim_fn_of_string: string => option(prim_fn) =
  fun
  | "add" => Some(Add)
  | "mult" => Some(Mult)
  | "fact" => Some(Fact)
  | _ => None;

let parse_atom: Block.word => form =
  fun
  | n when int_of_string_opt(n) != None => Lit(int_of_string(n))
  | v => Var(v);

let parse_expression: Block.words => form =
  fun
  | [] => Unknown
  | [x] => parse_atom(x)
  | [x, ...xs] =>
    switch (prim_fn_of_string(x)) {
    | Some(fn) => App(fn, List.map(parse_atom, xs))
    | _ =>
      switch (xs) {
      | ["+", x1] => App(Add, List.map(parse_atom, [x, x1]))
      | ["+", x1, "+", x2] => App(Add, List.map(parse_atom, [x, x1, x2]))
      | ["+", x1, "+", x2, "+", x3] =>
        App(Add, List.map(parse_atom, [x, x1, x2, x3]))
      | ["*", x1] => App(Mult, List.map(parse_atom, [x, x1]))
      | ["*", x1, "*", x2] => App(Mult, List.map(parse_atom, [x, x1, x2]))
      | ["*", x1, "*", x2, "*", x3] =>
        App(Mult, List.map(parse_atom, [x, x1, x2, x3]))
      | ["-", x1] => App(Add, List.map(parse_atom, [x, "-" ++ x1]))
      | ["-", x1, "-", x2] =>
        App(Add, List.map(parse_atom, [x, "-" ++ x1, "-" ++ x2]))
      | _ => Unknown
      }
    };

let parse_pattern: Block.words => option(string) =
  fun
  | [w] => Some(w)
  | _ => None;

let eval_atom: (form, env) => option(int) =
  (form, env) => {
    switch (form) {
    | Lit(n) => Some(n)
    | Var(v) =>
      switch (Environment.lookup(env, v)) {
      | None => None
      | Some(n) => n
      }
    | _ => None
    };
  };

let rec factorial = x =>
  if (x <= 2) {
    x;
  } else {
    x * factorial(x - 1);
  };

let rec eval_expression: (form, env) => option(int) =
  (form, env) => {
    switch (form) {
    | Unknown => None
    | Lit(n) => Some(n)
    | Var(v) =>
      switch (Environment.lookup(env, v)) {
      | None => None
      | Some(n) => n
      }
    | App(Add, xs) => bin_op((+), 0, env, xs)
    | App(Mult, xs) => bin_op(( * ), 1, env, xs)
    | App(Fact, [x]) => un_op(factorial, env, x)
    | App(Fact, _) => None
    };
  }
and bin_op = (int_op, int_op_identity, env, xs) =>
  List.fold_left(
    (acc, x) =>
      switch (acc, eval_expression(x, env)) {
      | (Some(n), Some(m)) => Some(int_op(n, m))
      | _ => None
      },
    Some(int_op_identity),
    xs,
  )
and un_op = (op, env, x) => Option.map(op, eval_expression(x, env));

let run_block: Block.t => Block.t =
  block =>
    List.fold_left(
      (
        (block_acc: Block.t, env_acc: env),
        {pattern, expression, _}: Block.cell,
      ) => {
        let parsed_exp = parse_expression(expression);
        let result = eval_expression(parsed_exp, env_acc);
        let new_env =
          switch (parse_pattern(pattern)) {
          | None => env_acc
          | Some(name) => Environment.extend(env_acc, (name, result))
          };
        let value =
          switch (result) {
          | None => ["?"]
          | Some(n) => [string_of_int(n)]
          };
        let new_cell: Block.cell = {pattern, expression, value};
        let new_block = block_acc @ [new_cell];
        (new_block, new_env);
      },
      ([], Environment.empty),
      block,
    )
    |> fst;

/*

 Idea: offset all word positions by tiny amounts in both x and y or just y?
 to make it look more fun. make this pseudorandom based on word as seed
 so it doesn't constantly change? but make it jiggle when selected (will naturally jiggle when typing)

  */
