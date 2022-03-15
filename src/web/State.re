open Sexplib.Std;

[@deriving sexp]
type tracked_elems = Core.Environment.t_(option(Model.box));

[@deriving sexp]
type t = {mutable tracked_elems};

let get_tracked_elems = (state: t): tracked_elems => state.tracked_elems;

let set_tracked_elems = (state: t, es: tracked_elems) =>
  state.tracked_elems = es;

let init_state: t = {tracked_elems: []};
