open Core;
open Sexplib.Std;
let cutoff = (===);

[@deriving sexp]
type focus =
  //TODO: revisit
  | SingleCell(Path.t);

[@deriving sexp]
type cell_proj =
  //| PatternOnly
  //| ValueOnly
  //| ExpressionPatternValue
  | ExpressionPattern;

[@deriving sexp]
type cell_sep_id = int;

//TODO: refactor to use path
[@deriving sexp]
type word_path = (Block.cell_id, Cell.field, Cell.word_idx);
[@deriving sexp]
type word_sep_path = word_path;
[@deriving sexp]
type word_sep_id = int;

[@deriving sexp]
type drop_target =
  | NoTarget
  | Word(word_path)
  | WordSeparator(word_sep_path)
  | CellSepatator(int);

[@deriving sexp]
type carry =
  | Nothing
  | Word(Path.t)
  | WordBrush(Word.t)
  | Cell(Path.t)
  | CellBrush(Cell.t);

[@deriving sexp]
type trash_item =
  | TrashedCell(Cell.t, (int, int))
  | TrashedWord(Word.t, (int, int));

[@deriving sexp]
type trash = list(trash_item);

[@deriving sexp]
type pattern_display =
  | Name
  | Emoji;

[@deriving sexp]
type keymap = {
  shift: bool,
  ctrl: bool,
};

[@deriving sexp]
type t = {
  world: Block.t,
  cell_proj,
  focus,
  trash,
  drop_target,
  carry,
  pattern_display,
  keymap,
};

let init_world: Block.t = [
  {
    pattern: ["bro"],
    expression: ["sum", "77", "5", "123"],
    value: ["205"],
  },
  {pattern: ["greeze"], expression: ["fact", "5"], value: ["120"]},
  {
    pattern: ["cloun"],
    expression: ["prod", "bro", "greeze"],
    value: ["24600"],
  },
  {
    pattern: ["foob"],
    expression: ["112", "+", "813", "+", "bro"],
    value: ["1135"],
  },
];
let init_path: Path.t = [
  Cell(Index(0, 4)),
  Field(Expression),
  Word(Index(0, 4)),
  Char(Index(0, 3)),
];

assert(Path.is_valid(init_world, init_path));

print_endline(
  Sexplib.Sexp.to_string_hum(
    AnnotatedBlock.sexp_of_annotated_block(AnnotatedBlock.mk(init_world)),
  ),
);

let init = {
  world: init_world,
  cell_proj: ExpressionPattern,
  focus: SingleCell(init_path),
  carry: Nothing,
  drop_target: NoTarget,
  trash: [],
  pattern_display: Name,
  keymap: {
    shift: false,
    ctrl: false,
  },
};
