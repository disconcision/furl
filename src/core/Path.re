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
