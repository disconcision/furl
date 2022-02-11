open Sexplib.Std;
open Util;

[@deriving sexp]
type t =
  | SwapCells(int, int)
  | PickupCell(int)
  | SetDraggedPath(Core.Block.path)
  | PickupWord(string)
  | InsertWord(Core.Block.path, int, string)
  | InsertCell(int, Core.Block.cell)
  | Delete(Core.Block.path)
  | DeleteFocussedWord
  | UpdateWord(Core.Block.path, string => string)
  | UpdateFocusedWord(string => string)
  | SetDropTarget(Model.drop_target)
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

let update_drop_target = (f, {drop_target, _} as model: Model.t) => {
  ...model,
  drop_target: f(drop_target),
};

let num_words_expression = (block, cell_idx) =>
  Core.Block.nth_cell(block, cell_idx)
  |> ((x: Core.Block.cell) => x.expression)
  |> List.length;

let is_only_word_empty_expression = (block, path, cell_idx) =>
  num_words_expression(block, cell_idx) == 1
  && Core.Block.get_word_path(path, block) == Some(Core.Block.empty_word);

let is_path_to_cell: Core.Block.path => bool = path => List.length(path) == 1;

let is_path_to_word: Core.Block.path => bool = path => List.length(path) == 3;

let rec apply: (Model.t, t, unit, ~schedule_action: 'a) => Model.t =
  (model: Model.t, update: t, state: State.t, ~schedule_action) => {
    let model =
      switch (update) {
      | SetFocus(focus) =>
        //TODO: check if previous focus is empty word, and if so delete it maybe?
        // do we distinguish between empty words and holes?
        update_focus(_ => focus, model)
      | SetDraggedPath(path) => update_dragged_path(_ => path, model)
      | SetDropTarget(target) => update_drop_target(_ => target, model)
      | PickupCell(idx) => update_carried_cell(_ => idx, model)
      | PickupWord(word) => update_carried_word(_ => word, model)
      | SwapCells(a, b) =>
        is_path_to_cell(model.dragged_path)
          ? update_world(ListUtil.swap(a, b), model) : model
      | Delete(path) =>
        print_endline("delete:");
        print_endline(
          Sexplib.Sexp.to_string_hum(Core.Block.sexp_of_path(path)),
        );
        switch (path) {
        | [Cell(Index(cell_idx, _)), Field(Expression), _, ..._]
            when is_only_word_empty_expression(model.world, path, cell_idx) => model
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
        | [Cell(Index(cell_idx, _))] =>
          update_world(ListUtil.remove(cell_idx), model)
        | _ => model
        };
      | DeleteFocussedWord =>
        switch (model.focus) {
        | SingleCell(path) =>
          apply(model, Delete(path), state, ~schedule_action)
        }
      | InsertCell(sep_idx, cell) =>
        let m = update_world(ListUtil.insert_at(sep_idx, cell), model);
        apply(
          m,
          SetFocus(
            SingleCell([
              Cell(Index(sep_idx, 666)),
              Field(Expression),
              Word(Index(0, 666)),
            ]),
          ), //TODO
          state,
          ~schedule_action,
        );
      | InsertWord(path, sep_idx, new_word) =>
        let model = update_drop_target(_ => NoTarget, model);
        // hack: sometimes ondragleave doesn't get triggered when dropping
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
        apply(
          m,
          switch (path) {
          | [c, f, ..._] =>
            SetFocus(SingleCell([c, f, Word(Index(sep_idx, 666))])) //TODO
          | _ => SetFocus(SingleCell(path))
          },
          state,
          ~schedule_action,
        );
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
    update_world(Core.Interpreter.run_block, model);
  };

/*
  TODO:

  put 'add' at top/left for dragging
  put '0' at same
  dragging on numbers changes instead of moves?

 create references by dragging names

   */
