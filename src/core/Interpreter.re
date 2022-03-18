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

[@deriving sexp]
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

let rec adjacent_pairs: list('a) => list(('a, 'a)) =
  fun
  | []
  | [_] => []
  | [a, b, ...cs] => [(a, b)] @ adjacent_pairs([b, ...cs]);

let rec bin_op_int_float =
        (
          int_op,
          float_op,
          a: option(Expression.lit),
          b: option(Expression.lit),
        ) => {
  switch (a, b) {
  | (Some(IntLit(i1)), Some(IntLit(i2))) => Some(int_op(i1, i2))
  | (Some(FloatLit(f1)), Some(FloatLit(f2))) => Some(float_op(f1, f2))
  /*| (Some(IntLit(i)), Some(FloatLit(f))) when Float.is_integer(f) =>
    Some(int_op(i, Int.of_float(f)))*/
  | (Some(IntLit(i)), Some(FloatLit(f))) =>
    Some(float_op(Float.of_int(i), f))
  | (Some(FloatLit(f)), Some(IntLit(i))) =>
    Some(float_op(f, Float.of_int(i)))
  | _ => None
  };
}
and fold_bin_op_int_float = (int_op, float_op, int_id: Expression.lit, xs) => {
  List.fold_left(bin_op_int_float(int_op, float_op), Some(int_id), xs);
}
and all = (xs: list(option(Expression.lit))) =>
  List.fold_left(
    (acc: option(Expression.lit), x: option(Expression.lit)) =>
      switch (acc, x) {
      | (Some(BoolLit(true)), Some(BoolLit(true))) =>
        Some(BoolLit(true))
      | _ => Some(BoolLit(false))
      },
    Some(BoolLit(true)),
    xs,
  )
and eval_expression: (env, Expression.t) => option(Expression.lit) =
  (env, form) => {
    switch (form) {
    | Unknown(_) => None
    | Atom(a) => eval_atom(a, env)
    | App(Not, xs) =>
      switch (xs) {
      | [x] =>
        switch (eval_expression(env, x)) {
        | Some(BoolLit(b)) => Some(BoolLit(!b))
        | _ => None
        }
      | _ => None
      }
    | App(Fact, xs) =>
      switch (xs) {
      | [x] =>
        switch (eval_expression(env, x)) {
        | Some(IntLit(n)) => Some(IntLit(factorial(n)))
        | _ => None
        }
      | _ => None
      }
    | App(Add, xs)
    | Seq(Add, xs) =>
      xs
      |> eval_all(env)
      |> fold_bin_op_int_float(
           (n, m) => IntLit(n + m),
           (n, m) => FloatLit(n +. m),
           IntLit(0),
         )
    | App(Mult, xs)
    | Seq(Mult, xs) =>
      xs
      |> eval_all(env)
      |> fold_bin_op_int_float(
           (n, m) => IntLit(n * m),
           (n, m) => FloatLit(n *. m),
           IntLit(1),
         )

    // TODO: below ops should be short-circuiting
    | Seq(And, xs)
    | App(And, xs) => xs |> eval_all(env) |> bin_op_bool((&&), true)
    | Seq(Or, xs)
    | App(Or, xs) => xs |> eval_all(env) |> bin_op_bool((||), false)
    | Seq(Equal, xs)
    | App(Equal, xs) =>
      xs
      |> eval_all(env)
      |> adjacent_pairs
      |> List.map(((a, b)) => Some(Expression.BoolLit(a == b)))
      |> all
    | Seq((MoreThan | LessThan) as op, xs)
    | App((MoreThan | LessThan) as op, xs) =>
      let (op_int, op_float) =
        switch (op) {
        | MoreThan =>
          let op = (n, m) => Expression.BoolLit(n > m);
          (op, op);
        | LessThan =>
          let op = (n, m) => Expression.BoolLit(n < m);
          (op, op);
        | _ => failwith("eval_expression impossible")
        };
      xs
      |> eval_all(env)
      |> adjacent_pairs
      |> List.map(((a, b)) => bin_op_int_float(op_int, op_float, a, b))
      |> all;
    | Seq(Not | Fact, _)
    | Let(_) => None
    };
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
          | Some(lit) => [Expression.string_of_lit(lit)]
          };
        let expression =
          List.map(
            ({word, _}: AnnotatedBlock.annotated_word_exp) => word,
            expression.words,
          );
        let new_block = block_acc @ [{pattern, expression, value, uid}];
        (new_block, new_env);
      },
      ([], Environment.empty),
      AnnotatedBlock.mk(block).cells,
    )
    |> fst;
