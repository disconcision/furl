open Sexplib.Std;

[@deriving sexp]
type word = string;
[@deriving sexp]
type words = list(word);

[@deriving sexp]
type box = {
  // bounding box
  height: int,
  width: int,
};

/*
 type props = {
   box
 };

 type props' = {
  box: option(box)
 }

 let init_props' = {
     box: None
 };
 */

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
type cell_id = int;

[@deriving sexp]
type attribute = int;
//  | Pattern //0
//  | Expression //1
//  | Value; //2

//type caret_position = int;

[@deriving sexp]
type index =
  | Index(int, int);

[@deriving sexp]
type path_frame =
  | Cell(index) // pos i out of k
  | Field(field)
  | Word(index)
  | Char(index);

[@deriving sexp]
type path = list(path_frame);

[@deriving sexp]
type block = list(cell);

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
  path, // empty
  cells: list(annotated_cell),
};

let rec is_valid_path: (block, path) => bool =
  (block, path) =>
    switch (path) {
    | [Cell(Index(current, total)), ...ps] =>
      List.length(block) == total
      && current < total
      && is_valid_path_cell(List.nth(block, current), ps)
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
      && is_valid_path_char(List.nth(words, current), ps)
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

[@deriving sexp]
type focus =
  | SingleCell(path /* cell_id, attribute, caret_position */);

[@deriving sexp]
type cell_proj =
  //| PatternOnly
  //| ValueOnly
  | ExpressionPattern;
//| ExpressionPatternValue;

[@deriving sexp]
type t = {
  world: block,
  cell_proj,
  focus,
  carried_cell: int,
  carried_word: string,
  dragged_path: path,
};

let init_world: block = [
  {pattern: ["blarg"], expression: ["blorgh", "blug"], value: ["blee"]},
  {
    pattern: ["fzerpoib"],
    expression: ["zhmoggle", "katriptic", "klugg"],
    value: ["dolen"],
  },
  {
    pattern: ["crork"],
    expression: ["gagen", "tal", "452"],
    value: ["bumkid"],
  },
];

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

let annotate_block: block => annotated_block =
  block => {
    let path = [];
    {
      path,
      cells: List.mapi(annotate_cell(path, List.length(block)), block),
    };
  };

let init_path = [
  Cell(Index(0, 3)),
  Field(Expression),
  Word(Index(0, 2)),
  Char(Index(0, 6)),
];

let init = {
  world: init_world,
  cell_proj: ExpressionPattern,
  focus: SingleCell(init_path),
  carried_cell: 0,
  carried_word: "",
  dragged_path: [],
  //attractor: (0,0)
};

print_endline(
  Sexplib.Sexp.to_string_hum(
    sexp_of_annotated_block(annotate_block(init_world)),
  ),
);

assert(is_valid_path(init_world, init_path));

/*
 selection b lik
 screen row <- proj row <- (cell id)
 (field id)
 (word index)
 (char pos)
  */

/*
 display:
 map cells to rows
  */

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
let rec get_path_to_coord =
        (block: block, (row: int, col: int)): get_path_res =>
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

/*

 want to:
   draw indications
     box around word
     box around line
   move indication with arrows (optional??)
   set indication on click
   insert word hole before/after another word on click
   new row (with word hole) on click

  */

let update_cell: (block, int, cell => cell) => block =
  (block, cell_idx, f) =>
    List.mapi((idx, cell) => idx == cell_idx ? f(cell) : cell, block);

let update_expression: (block, int, words => words) => block =
  (block, cell_idx, f) =>
    update_cell(block, cell_idx, ({expression, _} as cell) =>
      {...cell, expression: f(expression)}
    );

let update_pattern: (block, int, words => words) => block =
  (block, cell_idx, f) =>
    update_cell(block, cell_idx, ({pattern, _} as cell) =>
      {...cell, pattern: f(pattern)}
    );
