open Sexplib.Std;

[@deriving sexp]
type annotated_word_val = {
  path: Path.t,
  form: Value.atom,
  word: Word.t,
};

[@deriving sexp]
type annotated_word_pat = {
  path: Path.t,
  form: option(Pattern.atom),
  word: Word.t,
};

[@deriving sexp]
type annotated_word_exp = {
  path: Path.t,
  form: Expression.atom,
  word: Word.t,
};

[@deriving sexp]
type annotated_val = {
  path: Path.t,
  form: Value.t,
  words: list(annotated_word_val),
};

[@deriving sexp]
type annotated_pat = {
  path: Path.t,
  form: option(Pattern.t),
  words: list(annotated_word_pat),
};

[@deriving sexp]
type annotated_exp = {
  path: Path.t,
  form: Expression.t,
  words: list(annotated_word_exp),
};

[@deriving sexp]
type status =
  | Alive
  | Dead;

[@deriving sexp]
type vars = {
  bound_here: option(Pattern.uses_ctx),
  num_refs: option(int),
  binding_status: option(status),
  used_here: Path.ctx,
  context: Path.ctx,
};

[@deriving sexp]
type annotated_cell = {
  path: Path.t,
  vars,
  pattern: annotated_pat,
  expression: annotated_exp,
  value: annotated_val,
  uid: Cell.uid,
};

[@deriving sexp]
type t = {
  path: Path.t, // always empty for now since only one block
  cells: list(annotated_cell),
};

let annotate_word_val = (path, length, idx, word): annotated_word_val => {
  {
    //TODO: specialize parse for values
    path: path @ [Path.Word(Index(idx, length))],
    word,
    form: Value.parse_atom(word),
  };
};

let annotate_expression_word = (context, path, length, idx, word) => {
  {
    path: path @ [Path.Word(Index(idx, length))],
    form: Expression.parse_atom(context, word),
    word,
  };
};

let annotate_val: (Path.t, Word.s) => annotated_val =
  (path, words) => {
    {
      path,
      words: List.mapi(annotate_word_val(path, List.length(words)), words),
      form: Value.parse(words),
    };
  };

let annotate_pattern_word_forward =
    (path, length, idx, word): annotated_word_pat => {
  {
    path: path @ [Path.Word(Index(idx, length))],
    form: None, //will fill in reverse pass
    word,
  };
};

let annotate_pat: (Path.t, Word.s) => annotated_pat =
  (path, words) => {
    {
      path,
      words:
        List.mapi(
          annotate_pattern_word_forward(path, List.length(words)),
          words,
        ),
      form: None //will fill in reverse pass
    };
  };

let annotate_exp: (Path.ctx, Path.t, Word.s) => annotated_exp =
  (context, path, words) => {
    let form = Expression.parse(context, words);
    let words =
      List.mapi(
        annotate_expression_word(context, path, List.length(words)),
        words,
      );
    {path, form, words};
  };

let get_pat_vars: annotated_pat => Path.ctx =
  ({words, _}) =>
    List.map(({path, word, _}: annotated_word_pat) => (word, path), words);

let get_bound_exp_vars: annotated_exp => Path.ctx =
  ({words, _}) =>
    words
    |> List.filter(({form, _}: annotated_word_exp) =>
         Expression.is_bound_var(form)
       )
    |> List.map(({path, word, _}) => (word, path));

let annotate_cell: (Path.ctx, Path.t, int, int, Cell.t) => annotated_cell =
  (context, path, length, idx, {pattern, expression, value, uid}) => {
    let path = path @ [Cell(Index(idx, length))];
    let pattern = annotate_pat(path @ [Field(Pattern)], pattern);
    let expression =
      annotate_exp(context, path @ [Field(Expression)], expression);
    let value = annotate_val(path @ [Field(Value)], value);
    {
      path,
      vars: {
        num_refs: None,
        binding_status: None,
        bound_here: None, // Will fill in reverse pass
        used_here: get_bound_exp_vars(expression),
        context,
      },
      pattern,
      expression,
      value,
      uid,
    };
  };

let init_ctx = [
  ("sum", [Path.Cell(Index(-1, -1))]),
  ("prod", [Path.Cell(Index(-1, -1))]),
  ("fact", [Path.Cell(Index(-1, -1))]),
]; //TODO

let forward_pass: Block.t => t =
  block => {
    let path = [];

    let (cells, _) =
      Base.List.foldi(
        block,
        ~init=([], init_ctx),
        ~f=(idx, (acc_block, acc_ctx), cell) => {
          let ann_cell =
            annotate_cell(acc_ctx, path, List.length(block), idx, cell);
          let new_ctx =
            Environment.union(acc_ctx, get_pat_vars(ann_cell.pattern));
          (acc_block @ [ann_cell], new_ctx);
        },
      );
    {path, cells};
  };

let gather_uses: (Pattern.uses_ctx, annotated_exp) => Pattern.uses_ctx =
  (ctx, {words, _}) =>
    List.fold_left(
      (acc_ctx, {path, word, _}: annotated_word_exp) =>
        Environment.update_or_extend(
          acc_ctx,
          word,
          uses => uses @ [path],
          [path],
        ),
      ctx,
      words,
    );

let consume_uses:
  (Pattern.uses_ctx, annotated_pat) => (Pattern.uses_ctx, annotated_pat) =
  (co_ctx, {path, words, _}) => {
    let (new_words, new_ctx) =
      List.fold_left(
        ((acc_words, acc_ctx), {word, _} as ann_pat: annotated_word_pat) => {
          let form = Some(Pattern.parse_atom(acc_ctx, word));
          let new_words = acc_words @ [{...ann_pat, form}];
          let new_ctx =
            switch (Environment.lookup(acc_ctx, word)) {
            | Some(_) => Environment.update(acc_ctx, word, _ => [])
            | None => acc_ctx
            };
          (new_words, new_ctx);
        },
        ([], co_ctx),
        List.rev(words),
      );
    (
      new_ctx,
      {
        path,
        words: List.rev(new_words),
        form:
          //TODO: cleanup
          Some(
            Pattern.parse(
              new_ctx,
              List.map(({word, _}: annotated_word_pat) => word, new_words),
            ),
          ),
      },
    );
  };

let get_pat_var_uses: annotated_pat => Pattern.uses_ctx =
  ({words, _}) =>
    List.fold_left(
      (acc, {form, _}: annotated_word_pat) =>
        switch (form) {
        | Some(Var(name, uses)) => [(name, uses)] @ acc
        | _ => acc
        },
      Environment.empty,
      words,
    );

let count_uses: Pattern.uses_ctx => int =
  uses_ctx =>
    uses_ctx
    |> List.map(((_, x)) => List.length(x))
    |> List.fold_left((+), 0);

let init_live_ctx: list(annotated_cell) => list(Word.t) =
  reversed_block =>
    switch (reversed_block) {
    | [last_cell, ..._] =>
      List.map(
        ({word, _}: annotated_word_pat) => word,
        last_cell.pattern.words,
      )
    | _ => []
    };

let some_bound_here_in_live_ctx = (bound_here, live_ctx) =>
  List.exists(
    word => None != List.find_opt((==)(word), live_ctx),
    Environment.keys(bound_here),
  );

let cell_binding_status = (bound_here, live_ctx) =>
  some_bound_here_in_live_ctx(bound_here, live_ctx) ? Alive : Dead;

let reverse_annonate_cell:
  (list(Word.t), Pattern.uses_ctx, annotated_cell) =>
  (Pattern.uses_ctx, annotated_cell) =
  (live_ctx, co_ctx, {pattern, expression, vars, _} as ann_cell) => {
    let (co_ctx, pattern) = consume_uses(co_ctx, pattern);
    let co_ctx = gather_uses(co_ctx, expression);
    let uses = get_pat_var_uses(pattern);
    let vars = {
      ...vars,
      bound_here: Some(uses),
      num_refs: Some(count_uses(uses)),
      binding_status: Some(cell_binding_status(uses, live_ctx)),
    };
    (co_ctx, {...ann_cell, vars, pattern});
  };

let extend_live_ctx = (live_ctx, ann_cell) =>
  //TODO: this approach is probably bugged... need to account for shadowing,
  // remove things from live ctx. better approach: don't just blindly append,
  // make sure no duplicates, and the remove on encountering bindings.
  //ALSO: composite patterns are alive as long as 1 of their sub-patterns is
  switch (ann_cell.vars.binding_status, ann_cell.vars.used_here) {
  | (Some(Alive), uses_ctx) => live_ctx @ Environment.keys(uses_ctx)
  | _ => live_ctx
  };

let reverse_pass: t => t =
  ({cells, path, _}) => {
    let rev_cells = List.rev(cells);
    let (new_cells, _, _) =
      Base.List.fold(
        rev_cells,
        ~init=([], Environment.empty, init_live_ctx(rev_cells)),
        ~f=((acc_block, acc_ctx, live_ctx), cell) => {
          let (uses_ctx, ann_cell) =
            reverse_annonate_cell(live_ctx, acc_ctx, cell);
          let live_ctx = extend_live_ctx(live_ctx, ann_cell);
          (acc_block @ [ann_cell], uses_ctx, live_ctx);
        },
      );
    {cells: List.rev(new_cells), path};
  };

let mk = (block: Block.t): t => {
  block |> forward_pass |> reverse_pass;
};
