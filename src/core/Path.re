open Sexplib.Std;

[@deriving sexp]
type index =
  | Index(int, int);

[@deriving sexp]
type path_frame =
  | Cell(index)
  | Field(Cell.field)
  | Word(index)
  | Char(index);

[@deriving sexp]
type t = list(path_frame);

[@deriving sexp]
type ctx = Environment.t_(t);

let rec is_valid: (Block.t, t) => bool =
  (block, path) =>
    switch (path) {
    | [Cell(Index(current, total)), ...ps] =>
      List.length(block) == total
      && current < total
      && is_valid_path_cell(Block.nth_cell(block, current), ps)
    | _ =>
      print_endline("path invalid: block");
      false;
    }
and is_valid_path_cell: (Cell.t, t) => bool =
  (cell, path) =>
    switch (path) {
    | [Field(Pattern), ...ps] => is_valid_path_word(cell.pattern, ps)
    | [Field(Expression), ...ps] => is_valid_path_word(cell.expression, ps)
    | [Field(Value), ...ps] => is_valid_path_word(cell.value, ps)
    | _ =>
      print_endline("path invalid: cell");
      false;
    }
and is_valid_path_word: (Word.s, t) => bool =
  (words, path) =>
    switch (path) {
    | [Word(Index(current, total)), ...ps] =>
      List.length(words) == total
      && current < total
      && is_valid_path_char(Cell.nth_word(words, current), ps)
    | _ =>
      print_endline("path invalid: word");
      false;
    }
and is_valid_path_char: (Word.t, t) => bool =
  (word, path) =>
    switch (path) {
    | [Char(Index(current, total))] =>
      String.length(word) == total && current < total
    | _ =>
      print_endline("path invalid: char");
      false;
    };

let get_word: (t, Block.t) => option(Word.t) =
  (path, block) =>
    switch (path) {
    | [
        Cell(Index(cell_idx, _)),
        Field(field),
        Word(Index(word_idx, _)),
        ..._,
      ] =>
      Some(Block.get_word(cell_idx, field, word_idx, block))
    | _ => None
    };

let get_words: (t, Block.t) => option(Word.s) =
  (path, block) =>
    switch (path) {
    | [Cell(Index(cell_idx, _)), Field(field), ..._] =>
      Some(Block.get_words(cell_idx, field, block))
    | _ => None
    };

let get_num_words = (field_path, block) =>
  switch (get_words(field_path, block)) {
  | Some(words) => List.length(words)
  | None => 0
  };

let prev_word_path = (block: Cell.s, path: t): t =>
  switch (path) {
  | [c, f, Word(Index(n, k)), ...ps] when n >= 1 => [
      c,
      f,
      Word(Index(n - 1, k)),
      ...ps,
    ]
  | [c, Field(Expression), Word(Index(0, _)), ...ps] =>
    let pat_path = [c, Field(Pattern)];
    let length = get_num_words(pat_path, block);
    [c, Field(Pattern), Word(Index(length - 1, length)), ...ps];
  | [Cell(Index(i, k)), Field(Pattern), Word(Index(0, _)), ...ps]
      when i != 0 =>
    let prev_exp_path = [Cell(Index(i - 1, k)), Field(Expression)];
    let length = get_num_words(prev_exp_path, block);
    [
      Cell(Index(i - 1, k)),
      Field(Expression),
      Word(Index(length - 1, length)),
      ...ps,
    ];
  | _ => path
  };

let next_word_path = (block: Cell.s, path: t): t =>
  switch (path) {
  | [c, f, Word(Index(n, k)), ...ps] when n + 1 < k => [
      c,
      f,
      Word(Index(n + 1, k)),
      ...ps,
    ]
  | [c, Field(Pattern), Word(Index(_n, _k)), ...ps] =>
    let exp_path = [c, Field(Expression)];
    let length = get_num_words(exp_path, block);
    [c, Field(Expression), Word(Index(0, length)), ...ps];
  | [Cell(Index(i, k)), Field(Expression), Word(Index(_n, _k)), ...ps]
      when i + 1 < k =>
    let next_pat_path = [Cell(Index(i + 1, k)), Field(Pattern)];
    let length = get_num_words(next_pat_path, block);
    [
      Cell(Index(i + 1, k)),
      Field(Pattern),
      Word(Index(0, length)),
      ...ps,
    ];
  | _ => path
  };

let up_word_path = (block: Cell.s, path: t): t =>
  switch (path) {
  | [Cell(Index(i, k)), f, Word(Index(n, _k)), ...ps] when i != 0 =>
    let up_f_path = [Cell(Index(i - 1, k)), f];
    let length = get_num_words(up_f_path, block);
    let new_n = n > length - 1 ? length - 1 : n;
    [Cell(Index(i - 1, k)), f, Word(Index(new_n, length)), ...ps];
  | _ => path
  };

let down_word_path = (block: Cell.s, path: t): t =>
  switch (path) {
  | [Cell(Index(i, k)), f, Word(Index(n, _k)), ...ps] when i + 1 < k =>
    let down_f_path = [Cell(Index(i + 1, k)), f];
    let length = get_num_words(down_f_path, block);
    let new_n = n > length - 1 ? length - 1 : n;
    [Cell(Index(i + 1, k)), f, Word(Index(new_n, length)), ...ps];
  | _ => path
  };

let prev_word = (block: Cell.s, path: t): option(Word.t) =>
  get_word(prev_word_path(block, path), block);

let decr_word = (path: t): t =>
  switch (path) {
  | [c, _, Word(Index(0, _)), ..._] => [c]
  | [c, f, Word(Index(n, k)), ...ps] => [
      c,
      f,
      Word(Index(n - 1, k)),
      ...ps,
    ]
  | _ => path
  };
