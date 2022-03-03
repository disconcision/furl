open Core.Block;

type get_path_res =
  | EscapeAbove
  | EscapeBelow
  | EscapeLeft
  | EscapeRight
  | Path(Core.Path.t);

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
and get_path_to_coord_cell =
    (cell: Core.Cell.t, (row: int, col: int)): get_path_res => {
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
    (words: Core.Word.s, (_row: int, col: int)): get_path_res => {
  let get_word_index = (col: int, words: Core.Word.s) => {
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
