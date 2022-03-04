type env = Environment.t_(option(int));

let rec factorial = x =>
  if (x <= 2) {
    x;
  } else {
    x * factorial(x - 1);
  };

let eval_atom: (Expression.atom, env) => option(int) =
  (form, env) => {
    switch (form) {
    | Lit(n) => Some(n)
    | Var(v, _) =>
      switch (Environment.lookup(env, v)) {
      | None => None
      | Some(n) => n
      }
    | _ => None
    };
  };

let rec eval_expression: (Expression.form, env) => option(int) =
  (form, env) => {
    switch (form) {
    | Unknown(_) => None
    | Atom(a) => eval_atom(a, env)
    | App(Add, xs)
    | Seq(Add, xs) => bin_op((+), 0, env, xs)
    | App(Mult, xs)
    | Seq(Mult, xs) => bin_op(( * ), 1, env, xs)
    | App(Fact, [x]) => un_op(factorial, env, x)
    | _ => None
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
        {pattern, expression, _}: AnnotatedBlock.annotated_cell,
      ) => {
        let parsed_exp = expression.form;
        let result = eval_expression(parsed_exp, env_acc);
        // TODO: clean up this mess (pattern especially)
        let pattern =
          List.map(
            ({word, _}: AnnotatedBlock.annotated_word_pat) => word,
            pattern.words,
          );
        let new_env =
          switch (Pattern.parse([], pattern)) {
          | Atom(Var(name, _)) =>
            Environment.extend(env_acc, (name, result))
          | _ => env_acc
          };
        let value =
          switch (result) {
          | None => ["?"]
          | Some(n) => [string_of_int(n)]
          };
        let expression =
          List.map(
            ({word, _}: AnnotatedBlock.annotated_word_exp) => word,
            expression.words,
          );
        let new_cell: Cell.t = {pattern, expression, value};
        let new_block = block_acc @ [new_cell];
        (new_block, new_env);
      },
      ([], Environment.empty),
      AnnotatedBlock.mk(block).cells,
    )
    |> fst;
