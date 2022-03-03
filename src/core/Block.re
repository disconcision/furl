open Sexplib.Std;

[@deriving sexp]
type word = string;
[@deriving sexp]
type words = list(word);

[@deriving sexp]
type pattern = words;
[@deriving sexp]
type expression = words;
[@deriving sexp]
type value = words;

[@deriving sexp]
type field =
  | Pattern
  | Expression
  | Value;

[@deriving sexp]
type cell = {
  //id,
  pattern,
  expression,
  value,
};

[@deriving sexp]
type t = list(cell);

[@deriving sexp]
type cell_id = int;
[@deriving sexp]
type word_id = int;

let nth_cell: (t, cell_id) => cell = List.nth;
let nth_word: (words, word_id) => word = List.nth;

[@deriving sexp]
type index =
  | Index(int, int);

[@deriving sexp]
type path_frame =
  | Cell(index)
  | Field(field)
  | Word(index)
  | Char(index);

[@deriving sexp]
type path = list(path_frame);

[@deriving sexp]
type annotated_word = {
  path,
  word,
};

[@deriving sexp]
type annotated_field = {
  path,
  words: list(annotated_word),
};

[@deriving sexp]
type path_ctx = Environment.t_(path);

[@deriving sexp]
type vars = {
  bound_here: path_ctx,
  used_here: path_ctx,
};

[@deriving sexp]
type annotated_cell = {
  path,
  vars,
  pattern: annotated_field,
  expression: annotated_field,
  value: annotated_field,
};

[@deriving sexp]
type annotated_block = {
  path, // empty for now since only one block
  cells: list(annotated_cell),
};

let running_index = ref(0);
let _running_names = [
  "foo",
  "cruby",
  "baz",
  "crunk",
  "dree",
  "bar",
  "gruben",
  "bro",
  "freezepop",
  "weeb",
  "grug",
  "carrudy",
];
let running_names = [
  "📎",
  "🌽",
  "💭",
  "🌘",
  "🍸",
  "🎈",
  "🍋",
  "🐠",
  "🖍",
  "🤘",
  "🍮",
  "👌",
  "🌈",
  "🐿",
];

let empty_word: word = "🕳️";

let init_cell: 'a => cell =
  () => {
    let i = running_index^;
    running_index := running_index^ + 1;
    let name = List.nth(running_names, i mod List.length(running_names));
    {pattern: [name], expression: [empty_word], value: ["?"]};
  };

let rec is_valid_path: (t, path) => bool =
  (block, path) =>
    switch (path) {
    | [Cell(Index(current, total)), ...ps] =>
      List.length(block) == total
      && current < total
      && is_valid_path_cell(nth_cell(block, current), ps)
    | _ =>
      print_endline("path invalid: block");
      false;
    }
and is_valid_path_cell: (cell, path) => bool =
  (cell, path) =>
    switch (path) {
    | [Field(Pattern), ...ps] => is_valid_path_word(cell.pattern, ps)
    | [Field(Expression), ...ps] => is_valid_path_word(cell.expression, ps)
    | [Field(Value), ...ps] => is_valid_path_word(cell.value, ps)
    | _ =>
      print_endline("path invalid: cell");
      false;
    }
and is_valid_path_word: (words, path) => bool =
  (words, path) =>
    switch (path) {
    | [Word(Index(current, total)), ...ps] =>
      List.length(words) == total
      && current < total
      && is_valid_path_char(nth_word(words, current), ps)
    | _ =>
      print_endline("path invalid: word");
      false;
    }
and is_valid_path_char: (word, path) => bool =
  (word, path) =>
    switch (path) {
    | [Char(Index(current, total))] =>
      String.length(word) == total && current < total
    | _ =>
      print_endline("path invalid: char");
      false;
    };

let annotate_word = (path, length, idx, word) => {
  {path: path @ [Word(Index(idx, length))], word};
};

let annotate_field: (path, words) => annotated_field =
  (path, words) => {
    {
      path,
      words: List.mapi(annotate_word(path, List.length(words)), words),
    };
  };

let is_variable_name: word => bool =
  word =>
    switch (int_of_string_opt(word)) {
    | Some(_) => false
    | None => true
    };
//TODO: more

let get_pat_vars: annotated_field => path_ctx =
  ({words, _}) => List.map(({path, word, _}) => (word, path), words);
let get_exp_vars: annotated_field => path_ctx =
  ({words, _}) =>
    words
    |> List.map(({path, word, _}) => (word, path))
    |> List.filter(((word, _)) => is_variable_name(word));

let annotate_cell: (path, int, int, cell) => annotated_cell =
  (path, length, idx, {pattern, expression, value}) => {
    let path = path @ [Cell(Index(idx, length))];
    let pattern = annotate_field(path @ [Field(Pattern)], pattern);
    let expression = annotate_field(path @ [Field(Expression)], expression);
    let value = annotate_field(path @ [Field(Value)], value);
    {
      path,
      vars: {
        bound_here: get_pat_vars(pattern),
        used_here: get_exp_vars(expression),
      },
      pattern,
      expression,
      value,
    };
  };

let annotate_block: t => annotated_block =
  block => {
    let path = [];
    //let context = [];
    {
      path,
      cells: List.mapi(annotate_cell(path, List.length(block)), block),
    };
  };

let update_cell: (t, int, cell => cell) => t =
  (block, cell_idx, f) =>
    List.mapi((idx, cell) => idx == cell_idx ? f(cell) : cell, block);

let update_expression: (t, int, words => words) => t =
  (block, cell_idx, f) =>
    update_cell(block, cell_idx, ({expression, _} as cell) =>
      {...cell, expression: f(expression)}
    );

let update_pattern: (t, int, words => words) => t =
  (block, cell_idx, f) =>
    update_cell(block, cell_idx, ({pattern, _} as cell) =>
      {...cell, pattern: f(pattern)}
    );

let get_words: (int, field, t) => list(string) =
  (cell_idx, field, block) => {
    let cell = nth_cell(block, cell_idx);
    switch (field) {
    | Expression => cell.expression
    | Pattern => cell.pattern
    | Value => cell.value
    };
  };

let get_word: (int, field, int, t) => string =
  (cell_idx, field, word_idx, block) =>
    nth_word(get_words(cell_idx, field, block), word_idx);

let get_word_path: (path, t) => option(string) =
  (path, block) =>
    switch (path) {
    | [
        Cell(Index(cell_idx, _)),
        Field(field),
        Word(Index(word_idx, _)),
        ..._,
      ] =>
      Some(get_word(cell_idx, field, word_idx, block))
    | _ => None
    };

let get_words_path: (path, t) => option(words) =
  (path, block) =>
    switch (path) {
    | [Cell(Index(cell_idx, _)), Field(field), ..._] =>
      Some(get_words(cell_idx, field, block))
    | _ => None
    };

let update_word: (string => string, int, words) => words =
  (f, idx) => List.mapi((i, x) => i == idx ? f(x) : x);
