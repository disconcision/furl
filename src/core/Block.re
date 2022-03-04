open Sexplib.Std;

[@deriving sexp]
type t = Cell.s;

[@deriving sexp]
type cell_id = int;

let nth_cell: (t, cell_id) => Cell.t = List.nth;

let update_cell: (t, int, Cell.t => Cell.t) => t =
  (block, cell_idx, f) =>
    List.mapi((idx, cell) => idx == cell_idx ? f(cell) : cell, block);

let update_expression: (t, int, Word.s => Word.s) => t =
  (block, cell_idx, f) =>
    update_cell(block, cell_idx, ({expression, _} as cell) =>
      {...cell, expression: f(expression)}
    );

let update_pattern: (t, int, Word.s => Word.s) => t =
  (block, cell_idx, f) =>
    update_cell(block, cell_idx, ({pattern, _} as cell) =>
      {...cell, pattern: f(pattern)}
    );

let get_words: (int, Cell.field, t) => Word.s =
  (cell_idx, field, block) => {
    //TODO: opt check
    let cell = nth_cell(block, cell_idx);
    switch (field) {
    | Expression => cell.expression
    | Pattern => cell.pattern
    | Value => cell.value
    };
  };

let get_word: (int, Cell.field, int, t) => option(Word.t) =
  (cell_idx, field, word_idx, block) =>
    List.nth_opt(get_words(cell_idx, field, block), word_idx);
