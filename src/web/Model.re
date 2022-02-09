open Core;
open Sexplib.Std;

/*
 type t = {furl_model: FurlModel.t};

 let init = {furl_model: FurlModel.init};
 */

let cutoff = (===);

[@deriving sexp]
type focus =
  //TODO: revisit
  | SingleCell(World.path);

[@deriving sexp]
type cell_proj =
  //| PatternOnly
  //| ValueOnly
  //| ExpressionPatternValue
  | ExpressionPattern;

[@deriving sexp]
type t = {
  world: World.block,
  cell_proj,
  focus,
  carried_cell: int,
  carried_word: string,
  dragged_path: World.path,
};

let init_world: World.block = [
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
  world: World.init_world,
  cell_proj: ExpressionPattern,
  focus: SingleCell(World.init_path),
  carried_cell: 0,
  carried_word: "",
  dragged_path: [],
};
