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

let is_to_cell: t => bool =
  path =>
    switch (path) {
    | [Cell(_)] => true
    | _ => false
    };

let cell_idx = (path: t): option(Block.cell_id) =>
  switch (path) {
  | [Cell(Index(i, _)), ..._] => Some(i)
  | _ => None
  };

let is_cell_idx = (p, path: t) =>
  switch (path) {
  | [Cell(Index(i, _)), ..._] => p(i)
  | _ => false
  };

let update_cell_idx = (f, path: t): t =>
  switch (path) {
  | [Cell(Index(i, k)), ...ps] => [Cell(Index(f(i), k)), ...ps]
  | _ => path
  };

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

let get_cell: (t, Block.t) => option(Cell.t) =
  (path, block) =>
    switch (path) {
    | [Cell(Index(idx, _)), ..._] => Some(Block.nth_cell(block, idx))
    | _ => None
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
      Block.get_word(cell_idx, field, word_idx, block)
    | _ => None
    };

let get_words: (t, Block.t) => option(Word.s) =
  (path, block) =>
    switch (path) {
    | [Cell(Index(cell_idx, _)), Field(field), ..._] =>
      Some(Block.get_words(cell_idx, field, block)) //TODO: opt check
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
  /*
   | [Cell(Index(i, k)), Field(Pattern), Word(Index(0, _)), ..._]
       when i > 0 => [
       Cell(Index(i - 1, k)),
     ]
   | [Cell(Index(i, k))] =>
     let new_path = [Cell(Index(i, k)), Field(Expression)];
     let length = get_num_words(new_path, block);
     new_path @ [Word(Index(length - 1, length))];
   */
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
  /*
   | [Cell(Index(i, k)), Field(Expression), Word(Index(wn, wk)), ..._]
       when i + 1 < k && wn + 1 == wk => [
       Cell(Index(i + 1, k)),
     ]
   | [Cell(Index(i, k))] =>
     let new_path = [Cell(Index(i, k)), Field(Pattern)];
     let length = get_num_words(new_path, block);
     new_path @ [Word(Index(0, length))];
     */
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

let up_path = (block: Cell.s, path: t): t =>
  switch (path) {
  | [Cell(Index(n, k))] when n != 0 => [Cell(Index(n - 1, k))]
  | [Cell(Index(i, k)), f, Word(Index(n, _k)), ...ps] when i != 0 =>
    let up_f_path = [Cell(Index(i - 1, k)), f];
    let length = get_num_words(up_f_path, block);
    let new_n = n > length - 1 ? length - 1 : n;
    [Cell(Index(i - 1, k)), f, Word(Index(new_n, length)), ...ps];
  | _ => path
  };

let down_path = (block: Cell.s, path: t): t =>
  switch (path) {
  | [Cell(Index(n, k))] when n + 1 < k => [Cell(Index(n + 1, k))]
  | [Cell(Index(i, k)), f, Word(Index(n, _k)), ...ps] when i + 1 < k =>
    let down_f_path = [Cell(Index(i + 1, k)), f];
    let length = get_num_words(down_f_path, block);
    let new_n = n > length - 1 ? length - 1 : n;
    [Cell(Index(i + 1, k)), f, Word(Index(new_n, length)), ...ps];
  | _ => path
  };

let prev_word = (block: Cell.s, path: t): option(Word.t) =>
  get_word(prev_word_path(block, path), block);

let next_word = (block: Cell.s, path: t): option(Word.t) =>
  get_word(next_word_path(block, path), block);

let is_a_next_word = (block, path: t) =>
  switch (next_word(block, path)) {
  | Some(_) => true
  | _ => false
  };

let is_next_word_p = (p, block, path: t) =>
  switch (next_word(block, path)) {
  | Some(op) when p(op) => true
  | _ => false
  };

let is_prev_word_p = (p, block, path: t) =>
  switch (prev_word(block, path)) {
  | Some(op) when p(op) => true
  | _ => false
  };

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

let num_words_in_exp = (block, cell_idx) =>
  Block.nth_cell(block, cell_idx) |> (x => x.expression) |> List.length;

let is_exp_empty_word = (block, path, cell_idx) =>
  num_words_in_exp(block, cell_idx) == 1
  && get_word(path, block) == Some(Word.empty);

let delete: (t, Block.t) => Block.t =
  (path, block) => {
    switch (path) {
    //TODO: figure out why below was even a thing
    //| [Cell(Index(cell_idx, _)), Field(Expression), _, ..._]
    //    when is_exp_empty_word(block, path, cell_idx) => block
    | [
        Cell(Index(cell_idx, _)),
        Field(Expression),
        Word(Index(word_idx, _)),
        ..._,
      ] =>
      Block.update_expression(
        block,
        cell_idx,
        Util.ListUtil.remove(word_idx),
      )
    | [Cell(Index(cell_idx, _))] => Util.ListUtil.remove(cell_idx, block)
    | _ => block
    };
  };

let update_word: (Word.t => Word.t, t, Block.t) => Block.t =
  (f, path, block) =>
    switch (path) {
    | [
        Cell(Index(cell_idx, _)),
        Field(Expression),
        Word(Index(word_idx, _)),
        ..._,
      ] =>
      Block.update_expression(block, cell_idx, Cell.update_word(f, word_idx))
    | _ => block
    };

let insert_word: (Word.t, t, int, Block.t) => Block.t =
  (word, path, sep_idx, block) =>
    switch (path) {
    | [Cell(Index(cell_idx, _)), Field(Expression), ..._] =>
      Block.update_expression(
        block,
        cell_idx,
        Util.ListUtil.insert_at(sep_idx, word),
      )
    | _ => block
    };

let insert_cell: (int, Cell.t, Block.t) => Block.t = Util.ListUtil.insert_at;

/* FOCUSING
     these functions are used to focus down a subpath for a specific cell or word or whatever
   */

let focus_word = (path: t, word_idx: Cell.word_idx): option(t) =>
  switch (path) {
  | [Word(Index(idx, _)), ...subpath] =>
    word_idx == idx ? Some(subpath) : None
  | _ => None
  };

let focus_cell = (path: t, cell_idx: Block.cell_id): option(t) =>
  switch (path) {
  | [Cell(Index(idx, _)), ...subpath] =>
    cell_idx == idx ? Some(subpath) : None
  | _ => None
  };
