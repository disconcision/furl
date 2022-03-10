open Sexplib.Std;

[@deriving sexp]
type tracked_elems = Core.Environment.t_((int, int));

[@deriving sexp]
type t = {
  mutable tracked_elems,
  mutable anim_targets: list(string),
};

let init_coords = ((-1), (-1));

let cell_targets_todo = [
  "-1",
  "0",
  "1",
  "2",
  "3",
  "4",
  "5",
  "6",
  "7",
  "8",
  "9",
];

let init_state: t = {
  tracked_elems: List.map(id => (id, init_coords), cell_targets_todo),
  anim_targets: [],
};
