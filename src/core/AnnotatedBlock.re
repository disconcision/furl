open Sexplib.Std;
open Path;

[@deriving sexp]
type annotated_word = {
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
type vars = {
  bound_here: Path.ctx,
  used_here: Path.ctx,
  context: Path.ctx,
};

[@deriving sexp]
type annotated_cell = {
  path: Path.t,
  vars,
  pattern: annotated_field,
  expression: annotated_field,
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
    path: path @ [Word(Index(idx, length))],
    word,
    form: Formless(word),
  };
};

let annotate_expression_word = (context, path, length, idx, word) => {
  {
    path: path @ [Word(Index(idx, length))],
    form: Expression.parse_exp_atom(context, word),
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

let annotate_pat = annotate_field;
let annotate_val = annotate_field;

let annotate_exp: (Path.ctx, Path.t, Word.s) => annotated_field =
  (context, path, words) => {
    let words =
      List.mapi(
        annotate_expression_word(context, path, List.length(words)),
        words,
      );
    {path, words};
  };

let get_pat_vars: annotated_field => Path.ctx =
  //TODO: update when patterns
  ({words, _}) => List.map(({path, word, _}) => (word, path), words);

let get_bound_exp_vars: annotated_field => Path.ctx =
  ({words, _}) =>
    words
    |> List.filter(({form, _}) => Expression.is_bound_var(form))
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
        bound_here: get_pat_vars(pattern),
        used_here: get_bound_exp_vars(expression),
        context,
      },
      pattern,
      expression,
      value,
    };
  };

let mk: Block.t => annotated_block =
  block => {
    let path = [];
    let init_context = [];
    let (cells, _) =
      Base.List.foldi(
        block,
        ~init=([], init_context),
        ~f=(idx, (acc_block, acc_ctx), cell) => {
          let ann_cell =
            annotate_cell(acc_ctx, path, List.length(block), idx, cell);
          (
            acc_block @ [ann_cell],
            Environment.union(acc_ctx, ann_cell.vars.bound_here),
          );
        },
      );
    {
      path,
      cells // List.mapi(annotate_cell(path, List.length(block)), block),
    };
  };
