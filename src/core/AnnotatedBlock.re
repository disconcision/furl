open Sexplib.Std;

[@deriving sexp]
type annotated_word = {
  path: Path.t,
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
type annotated_field = {
  path: Path.t,
  words: list(annotated_word),
};

[@deriving sexp]
type annotated_pat = {
  path: Path.t,
  form: option(Pattern.form),
  words: list(annotated_word_pat),
};

[@deriving sexp]
type annotated_exp = {
  path: Path.t,
  form: Expression.form,
  words: list(annotated_word_exp),
};

[@deriving sexp]
type vars = {
  bound_here: option(Pattern.uses_ctx),
  used_here: Path.ctx,
  context: Path.ctx,
};

[@deriving sexp]
type annotated_cell = {
  path: Path.t,
  vars,
  pattern: annotated_pat,
  expression: annotated_exp,
  value: annotated_field,
};

[@deriving sexp]
type annotated_block = {
  path: Path.t, // always empty for now since only one block
  cells: list(annotated_cell),
};

let annotate_word = (path, length, idx, word) => {
  {
    //TODO: specialize parse for patterns, values
    path: path @ [Path.Word(Index(idx, length))],
    word,
  };
};

let annotate_expression_word = (context, path, length, idx, word) => {
  {
    path: path @ [Path.Word(Index(idx, length))],
    form: Expression.parse_atom(context, word),
    word,
  };
};

let annotate_field: (Path.t, Word.s) => annotated_field =
  (path, words) => {
    {
      path,
      words: List.mapi(annotate_word(path, List.length(words)), words),
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

let annotate_val = annotate_field;

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
  (context, path, length, idx, {pattern, expression, value}) => {
    let path = path @ [Cell(Index(idx, length))];
    let pattern = annotate_pat(path @ [Field(Pattern)], pattern);
    let expression =
      annotate_exp(context, path @ [Field(Expression)], expression);
    let value = annotate_val(path @ [Field(Value)], value);
    {
      path,
      vars: {
        bound_here: None, // Will fill in reverse pass
        used_here: get_bound_exp_vars(expression),
        context,
      },
      pattern,
      expression,
      value,
    };
  };

let forward_pass: Block.t => annotated_block =
  block => {
    let path = [];
    let init_ctx = Environment.empty;
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
  (co_ctx, {path, words, _} as ann_pat) => {
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
    (new_ctx, {...ann_pat, path, words: List.rev(new_words)});
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

let reverse_annonate_cell:
  (Pattern.uses_ctx, annotated_cell) => (Pattern.uses_ctx, annotated_cell) =
  (co_ctx, {pattern, expression, vars, _} as ann_cell) => {
    let co_ctx = gather_uses(co_ctx, expression);
    let (co_ctx, pattern) = consume_uses(co_ctx, pattern);
    let vars = {...vars, bound_here: Some(get_pat_var_uses(pattern))};
    (co_ctx, {...ann_cell, vars, pattern});
  };

let reverse_pass: annotated_block => annotated_block =
  ({cells, path, _}) => {
    let (new_cells, _) =
      Base.List.fold(
        List.rev(cells),
        ~init=([], Environment.empty),
        ~f=((acc_block, acc_ctx), cell) => {
          let (uses_ctx, ann_cell) = reverse_annonate_cell(acc_ctx, cell);
          (acc_block @ [ann_cell], uses_ctx);
        },
      );
    {cells: List.rev(new_cells), path};
  };

let mk = (block: Block.t): annotated_block => {
  block |> forward_pass |> reverse_pass;
};
