type env = Environment.t_(option(Expression.lit));

let rec factorial = x =>
  if (x <= 2) {
    x;
  } else {
    x * factorial(x - 1);
  };

let eval_atom: (Expression.atom, env) => option(Expression.lit) =
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

type ty =
  | Bool
  | Int
  | Float
  | UnknownType;

let ty_of_lit: Expression.lit => ty =
  fun
  | BoolLit(_) => Bool
  | IntLit(_) => Int
  | FloatLit(_) => Float
  | Indet(_) => UnknownType;

let all_types_are = (ty: ty, xs: list(Expression.lit)) =>
  List.for_all(lit => lit |> ty_of_lit |> (==)(ty), xs);

let rec eval_expression: (env, Expression.t) => option(Expression.lit) =
  (env, form) => {
    switch (form) {
    | Unknown(_) => None
    | Atom(a) => eval_atom(a, env)
    | App(Not, [x]) =>
      switch (eval_expression(env, x)) {
      | Some(BoolLit(b)) => Some(BoolLit(!b))
      | _ => None
      }
    | App(Not, _) => None
    | App(Add, xs)
    | Seq(Add, xs) => xs |> eval_all(env) |> bin_op_int_float((+), (+.), 0)
    | App(Mult, xs)
    | Seq(Mult, xs) =>
      xs |> eval_all(env) |> bin_op_int_float(( * ), ( *. ), 1)
    | App(Fact, [x]) =>
      switch (eval_expression(env, x)) {
      | Some(IntLit(n)) => Some(IntLit(factorial(n)))
      | _ => None
      }
    | App(Fact, _)
    | Seq(Not | Fact, _)
    | Let(_) => None
    | Seq(And, xs)
    | App(And, xs) => xs |> eval_all(env) |> bin_op_bool((&&), true)
    | Seq(Or, xs)
    | App(Or, xs) => xs |> eval_all(env) |> bin_op_bool((||), false)
    //| App(And | Or, _) => failwith("TODO eval_expression")
    };
  }
and bin_op_int_float = (int_op, float_op, int_id, xs) => {
  List.fold_left(
    (acc: option(Expression.lit), v: option(Expression.lit)) =>
      switch (acc, v) {
      | (Some(IntLit(n)), Some(IntLit(m))) => Some(IntLit(int_op(n, m)))
      | (Some(IntLit(n)), Some(FloatLit(f))) when Float.is_integer(f) =>
        Some(IntLit(int_op(n, Int.of_float(f))))
      | (Some(FloatLit(n)), Some(FloatLit(m))) =>
        Some(FloatLit(float_op(n, m)))
      | (Some(FloatLit(n)), Some(IntLit(i)))
      | (Some(IntLit(i)), Some(FloatLit(n))) =>
        Some(FloatLit(float_op(n, Float.of_int(i))))
      | _ => None
      },
    Some(IntLit(int_id)),
    xs,
  );
}
and bin_op_bool = (bool_op, bool_id, xs) => {
  List.fold_left(
    (acc: option(Expression.lit), v: option(Expression.lit)) =>
      switch (acc, v) {
      | (Some(BoolLit(n)), Some(BoolLit(m))) =>
        Some(BoolLit(bool_op(n, m)))
      | _ => None
      },
    Some(BoolLit(bool_id)),
    xs,
  );
}
and bin_op = (int_op, int_op_identity, env, xs) =>
  List.fold_left(
    (acc, x) =>
      switch (acc, eval_expression(env, x)) {
      | (Some(n), Some(m)) => Some(int_op(n, m))
      | _ => None
      },
    Some(int_op_identity),
    xs,
  )

and eval_all: (env, list(Expression.t)) => list(option(Expression.lit)) =
  (env, xs) => xs |> List.map(eval_expression(env))
and eval_all_opt: (env, list(Expression.t)) => option(list(Expression.lit)) =
  (env, xs) => xs |> eval_all(env) |> Util.OptUtil.sequence;

let run_block: Block.t => Block.t =
  block =>
    List.fold_left(
      (
        (block_acc: Block.t, env_acc: env),
        {pattern, expression, uid, _}: AnnotatedBlock.annotated_cell,
      ) => {
        let parsed_exp = expression.form;
        let result = eval_expression(env_acc, parsed_exp);
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
          | Some(lit) =>
            switch (lit) {
            | BoolLit(b) => [string_of_bool(b)]
            | IntLit(n) => [string_of_int(n)]
            | FloatLit(f) => [string_of_float(f)]
            | Indet(_) => ["??"]
            }
          };
        let expression =
          List.map(
            ({word, _}: AnnotatedBlock.annotated_word_exp) => word,
            expression.words,
          );
        let new_cell: Cell.t = {pattern, expression, value, uid};
        let new_block = block_acc @ [new_cell];
        (new_block, new_env);
      },
      ([], Environment.empty),
      AnnotatedBlock.mk(block).cells,
    )
    |> fst;
