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

//TODO: refactor to use path
[@deriving sexp]
type word_path = (Block.cell_id, Cell.field, Cell.word_idx);
[@deriving sexp]
type word_sep_path = word_path;
[@deriving sexp]
type cell_sep_path = int;
[@deriving sexp]
type drop_target =
  | NoTarget
  | Word(word_path)
  | WordSeparator(word_sep_path)
  | CellSepatator(int);

[@deriving sexp]
type trash_item =
  | TrashedWord(string, (int, int));

[@deriving sexp]
type trash = list(trash_item);

[@deriving sexp]
type t = {
  world: Block.t,
  cell_proj,
  focus,
  trash,
  drop_target,
  carried_cell: int,
  carried_word: string,
  dragged_path: Path.t,
};

let init_world: Block.t = [
  {
    pattern: ["blarg"],
    expression: ["add", "77", "5", "123"],
    value: ["205"],
  },
  {pattern: ["freezepop"], expression: ["fact", "5"], value: ["120"]},
  {
    pattern: ["crork"],
    expression: ["mult", "blarg", "freezepop"],
    value: ["24600"],
  },
];
let init_path: Path.t = [
  Cell(Index(0, 3)),
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
  carried_cell: 0,
  carried_word: "",
  dragged_path: [],
  drop_target: NoTarget,
  trash: [],
};
