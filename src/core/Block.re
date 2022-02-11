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
type annotated_cell = {
  path,
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
let running_names = [
  "foo",
  "crobe",
  "baz",
  "clu",
  "dree",
  "gar",
  "bro",
  "wee",
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

let annotate_field: (path, words) => annotated_field =
  (path, words) => {
    {
      path,
      words:
        List.mapi(
          (idx, word) => {
            {path: path @ [Word(Index(idx, List.length(words)))], word}
          },
          words,
        ),
    };
  };

let annotate_cell: (path, int, int, cell) => annotated_cell =
  (path, length, idx, {pattern, expression, value}) => {
    let path = path @ [Cell(Index(idx, length))];
    {
      path,
      pattern: annotate_field(path @ [Field(Pattern)], pattern),
      expression: annotate_field(path @ [Field(Expression)], expression),
      value: annotate_field(path @ [Field(Value)], value),
    };
  };

let annotate_block: t => annotated_block =
  block => {
    let path = [];
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

type get_path_res =
  | EscapeAbove
  | EscapeBelow
  | EscapeLeft
  | EscapeRight
  | Path(path);

let col_1_width = 20;
let col_2_width = 60;
let col_3_width = 10;

/*
 initially assume:
   all cells are laid out vertically in a block
   all cells are 1 row high
   all field-cols have a fixed width (space-padded on the right)
   all fields are 1 row high
   all words are laid out horizontally in a field
   all words are 1 row high
 */
let rec get_path_to_coord = (block: t, (row: int, col: int)): get_path_res =>
  switch (row, List.length(block)) {
  | (r, _) when r < 0 => EscapeAbove
  | (r, n) when r >= n => EscapeBelow
  | (r, n) =>
    let cell = List.nth(block, row);
    switch (get_path_to_coord_cell(cell, (row, col))) {
    | Path(ps) => Path([Cell(Index(r, n)), ...ps])
    | x => x
    };
  }
and get_path_to_coord_cell = (cell: cell, (row: int, col: int)): get_path_res => {
  switch (col) {
  | c when c < 0 => EscapeLeft
  | c when c < col_1_width =>
    switch (get_path_to_coord_word(cell.pattern, (row, col))) {
    | Path(ps) => Path([Field(Pattern), ...ps])
    | x => x
    }
  | c when c >= col_1_width && c < col_2_width =>
    switch (
      get_path_to_coord_word(cell.expression, (row, col - col_1_width))
    ) {
    | Path(ps) => Path([Field(Expression), ...ps])
    | x => x
    }
  | c when c >= col_2_width && c < col_3_width =>
    switch (
      get_path_to_coord_word(
        cell.value,
        (row, col - col_1_width - col_2_width),
      )
    ) {
    | Path(ps) => Path([Field(Value), ...ps])
    | x => x
    }
  | c when c >= col_3_width => EscapeRight
  | _ => failwith("get_path_to_coord_cell")
  };
}
and get_path_to_coord_word =
    (words: words, (_row: int, col: int)): get_path_res => {
  let get_word_index = (col: int, words: list(word)) => {
    let gap_constant = 1;
    let rec gwi = (col, words, index: int, c: int) => {
      switch (words) {
      | [] => None
      | [w, ...ws] =>
        let start_int = c;
        let len = String.length(w);
        let end_int = c + len + gap_constant;
        col >= start_int && col < end_int
          ? Some((index + 1, col - start_int, len))
          : gwi(col, ws, index + 1, end_int);
      };
    };
    gwi(col, words, -1, 0);
  };
  let num_words = List.length(words);
  switch (get_word_index(col, words)) {
  | None when num_words == 0 => EscapeRight
  | None =>
    let last_word = List.hd(List.rev(words));
    let len = String.length(last_word);
    Path([
      Word(Index(num_words - 1, num_words)),
      Char(Index(len - 1, len)),
    ]);
  //| None => EscapeRight // TODO: prob just want to round down to last index here
  | Some(((-1), _, _)) => EscapeLeft
  | Some((w, c, wlen)) =>
    Path([Word(Index(w, num_words)), Char(Index(c, wlen))])
  };
};
