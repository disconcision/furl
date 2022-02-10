open Sexplib.Std;
open Util;

[@deriving sexp]
type t =
  | SwapCells(int, int)
  | PickupCell(int)
  | SetDraggedPath(Core.Block.path)
  | PickupWord(string)
  | InsertWord(Core.Block.path, int, string)
  | Delete(Core.Block.path)
  | DeleteFocussedWord
  | UpdateWord(Core.Block.path, string => string)
  | UpdateFocusedWord(string => string)
  | SetFocus(Model.focus);

let update_focus = (f, {focus, _} as model: Model.t) => {
  ...model,
  focus: f(focus),
};

let update_world = (f, {world, _} as model: Model.t) => {
  ...model,
  world: f(world),
};

let update_carried_cell = (f, {carried_cell, _} as model: Model.t) => {
  ...model,
  carried_cell: f(carried_cell),
};

let update_carried_word = (f, {carried_word, _} as model: Model.t) => {
  ...model,
  carried_word: f(carried_word),
};

let update_dragged_path = (f, {dragged_path, _} as model: Model.t) => {
  ...model,
  dragged_path: f(dragged_path),
};

let rec apply: (Model.t, t, unit, ~schedule_action: 'a) => Model.t =
  (model: Model.t, update: t, state: State.t, ~schedule_action) =>
    switch (update) {
    | SetFocus(focus) => update_focus(_ => focus, model)
    | SetDraggedPath(path) => update_dragged_path(_ => path, model)
    | PickupCell(idx) => update_carried_cell(_ => idx, model)
    | PickupWord(word) => update_carried_word(_ => word, model)
    | SwapCells(a, b) => update_world(ListUtil.swap(a, b), model)
    | Delete(path) =>
      switch (path) {
      | [
          Cell(Index(cell_idx, _)),
          Field(Expression),
          Word(Index(word_idx, _)),
          ..._,
        ] =>
        update_world(
          world =>
            Core.Block.update_expression(
              world,
              cell_idx,
              ListUtil.remove(word_idx),
            ),
          model,
        )
      | [Cell(Index(cell_idx, _)), ..._] =>
        update_world(ListUtil.remove(cell_idx), model)
      | _ => model
      }
    | DeleteFocussedWord =>
      switch (model.focus) {
      | SingleCell(path) =>
        apply(model, Delete(path), state, ~schedule_action)
      }
    | InsertWord(path, sep_idx, new_word) =>
      let m =
        update_world(
          world =>
            switch (path) {
            | [Cell(Index(cell_idx, _)), Field(Expression), ..._] =>
              Core.Block.update_expression(
                world,
                cell_idx,
                ListUtil.insert_at(sep_idx, new_word),
              )
            | _ => world
            },
          model,
        );
      apply(m, SetFocus(SingleCell(path)), state, ~schedule_action);
    | UpdateFocusedWord(f) =>
      switch (model.focus) {
      | SingleCell(path) =>
        apply(model, UpdateWord(path, f), state, ~schedule_action)
      }
    | UpdateWord(path, f) =>
      update_world(
        world =>
          switch (path) {
          | [
              Cell(Index(cell_idx, _)),
              Field(Expression),
              Word(Index(word_idx, _)),
              ..._,
            ] =>
            Core.Block.update_expression(
              world,
              cell_idx,
              Core.Block.update_word(f, word_idx),
            )
          | _ => world
          },
        model,
      )
    };
