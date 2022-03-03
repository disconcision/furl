open Sexplib.Std;
open Path;

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
type annotated_exp = {
  path: Path.t,
  form: Expression.form,
  words: list(annotated_word_exp),
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
    path: path @ [Word(Index(idx, length))],
    word,
  };
};

let annotate_expression_word = (context, path, length, idx, word) => {
  {
    path: path @ [Word(Index(idx, length))],
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

let annotate_pat = annotate_field;
let annotate_val = annotate_field;

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

let get_pat_vars: annotated_field => Path.ctx =
  //TODO: update when composite patterns
  ({words, _}) =>
    List.map(({path, word}: annotated_word) => (word, path), words);

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
    let init_ctx = Environment.empty;
    let (cells, _) =
      Base.List.foldi(
        block,
        ~init=([], init_ctx),
        ~f=(idx, (acc_block, acc_ctx), cell) => {
          let ann_cell =
            annotate_cell(acc_ctx, path, List.length(block), idx, cell);
          let new_ctx = Environment.union(acc_ctx, ann_cell.vars.bound_here);
          (acc_block @ [ann_cell], new_ctx);
        },
      );
    {path, cells};
  };

let reannotate_cell: (Pattern.uses_ctx, annotated_cell) => annotated_cell =
  (_coctx, ann_cell) => {
    //get var uses
    ann_cell;
  };

let reverse_pass: annotated_block => annotated_block =
  ann_block => {
    let init_co_ctx = Environment.empty;
    let {cells, path} = ann_block;
    let (new_cells, _) =
      Base.List.fold(
        List.rev(cells),
        ~init=([], init_co_ctx),
        ~f=((acc_block, acc_ctx), cell) => {
          let ann_cell = reannotate_cell(acc_ctx, cell);
          let new_co_ctx = init_co_ctx; //TODO!!!!!
          (acc_block @ [ann_cell], new_co_ctx);
        },
      );
    {cells: List.rev(new_cells), path};
  };
