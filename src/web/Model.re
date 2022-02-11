open Core;
open Sexplib.Std;
let cutoff = (===);

[@deriving sexp]
type focus =
  //TODO: revisit
  | SingleCell(Block.path);

[@deriving sexp]
type cell_proj =
  //| PatternOnly
  //| ValueOnly
  //| ExpressionPatternValue
  | ExpressionPattern;

[@deriving sexp]
type word_sep_path = (Block.cell_id, Block.field, int);
[@deriving sexp]
type cell_sep_path = int;
[@deriving sexp]
type drop_target =
  | NoTarget
  | WordSeparator(word_sep_path);

[@deriving sexp]
type t = {
  world: Block.t,
  cell_proj,
  focus,
  drop_target,
  carried_cell: int,
  carried_word: string,
  dragged_path: Block.path,
};

let init_world: Block.t = [
  {pattern: ["blarg"], expression: ["blorgh", "blug"], value: ["?"]},
  {
    pattern: ["freezepop"],
    expression: ["zhmoggle", "katriptic", "klugg"],
    value: ["?"],
  },
  {
    pattern: ["crork"],
    expression: ["gagen", "eminem", "452"],
    value: ["?"],
  },
];
let init_path: Block.path = [
  Cell(Index(0, 3)),
  Field(Expression),
  Word(Index(0, 2)),
  Char(Index(0, 6)),
];

assert(Block.is_valid_path(init_world, init_path));

print_endline(
  Sexplib.Sexp.to_string_hum(
    Block.sexp_of_annotated_block(Block.annotate_block(init_world)),
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
};
