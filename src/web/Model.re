open Core;
open Sexplib.Std;
let cutoff = (===);

[@deriving sexp]
type focus =
  | SingleCell(Path.t);

[@deriving sexp]
type word_sep_id = int;
[@deriving sexp]
type cell_sep_id = int;

//TODO: refactor paths
[@deriving sexp]
type word_path = (Block.cell_id, Cell.field, Cell.word_idx);
//[@deriving sexp]
//type char_path = (Block.cell_id, Cell.field, Cell.word_idx, int);

type new_path =
  | BlockPath
  | CellPath(Block.cell_id)
  | WordPath(word_path);
//| CharPath(char_path);

[@deriving sexp]
type drop_target =
  | NoTarget
  | Word(word_path)
  | WordSeparator(word_path)
  | CellSepatator(cell_sep_id);

[@deriving sexp]
type carry =
  | NoCarry
  | WordExp(AnnotatedBlock.annotated_word_exp)
  | WordPat(AnnotatedBlock.annotated_word_pat)
  | WordBrush(Word.t)
  | Cell(AnnotatedBlock.annotated_cell)
  | CellBrush(Cell.t);

[@deriving sexp]
type trash_idx = int;

[@deriving sexp]
type screen_coords = (int, int);

[@deriving sexp]
type trash_item =
  | TrashedCell(Cell.t, screen_coords)
  | TrashedWord(Word.t, screen_coords);

[@deriving sexp]
type trash = list(trash_item);

[@deriving sexp]
type pattern_display =
  | Name
  | Emoji;

[@deriving sexp]
type cell_proj =
  //| PatternOnly
  //| ValueOnly
  //| ExpressionPatternValue
  | ExpressionPattern;

[@deriving sexp]
type keymap = {
  shift: bool,
  ctrl: bool,
};

[@deriving sexp]
type lastkey =
  | KeyUp(string)
  | KeyDown(string);

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
  animations_off: bool,
  lastkey,
};

let world = World.init;

let init_path: Path.t =
  Lens.[
    Cell(Index(0, Block.len(world))),
    Field(Expression),
    Word(Index(0, first_exp_len(world))),
    Char(Index(0, first_word_len(world))),
  ];

assert(Path.is_valid(world, init_path));

let init = {
  world,
  focus: SingleCell(init_path),
  carry: NoCarry,
  drop_target: NoTarget,
  trash: [],
  keymap: {
    shift: false,
    ctrl: false,
  },
  pattern_display: Name,
  cell_proj: ExpressionPattern,
  animations_off: false,
  lastkey: KeyUp(""),
};
