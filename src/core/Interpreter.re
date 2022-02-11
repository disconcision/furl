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
  | Mult;

type form =
  | Lit(int)
  | Var(string)
  | App(prim_fn, list(form))
  | Unknown;

let parse_atom: Block.word => form =
  fun
  | n when int_of_string_opt(n) != None => Lit(int_of_string(n))
  | v => Var(v);

let parse_expression: Block.words => form =
  fun
  | [] => Unknown
  | [x] => parse_atom(x)
  | [x, ...xs] =>
    switch (x) {
    | "add"
    | "Add" => App(Add, List.map(parse_atom, xs))
    | "mult"
    | "Mult" => App(Mult, List.map(parse_atom, xs))
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

let int_op: (prim_fn, int, int) => int =
  fun
  | Add => ((x, y) => x + y)
  | Mult => ((x, y) => x * y);

let int_op_identity: prim_fn => int =
  fun
  | Add => 0
  | Mult => 1;

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
    | App(op, xs) =>
      List.fold_left(
        (acc, x) =>
          switch (acc, eval_expression(x, env)) {
          | (None, _)
          | (_, None) => None
          | (Some(n), Some(m)) => Some(int_op(op, n, m))
          },
        Some(int_op_identity(op)),
        xs,
      )
    };
  };

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
