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
type t = {
  world: Block.t,
  cell_proj,
  focus,
  carried_cell: int,
  carried_word: string,
  dragged_path: Block.path,
};

let init_world: Block.t = [
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

let init = {
  world: Block.init_world,
  cell_proj: ExpressionPattern,
  focus: SingleCell(Block.init_path),
  carried_cell: 0,
  carried_word: "",
  dragged_path: [],
};
