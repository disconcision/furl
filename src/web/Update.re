open Sexplib.Std;
open Util;
open Core;

[@deriving sexp]
type t =
  | SwapCells(int, int)
  | AddWordToTrash(Word.t, (int, int))
  | EmptyTrash
  | PickupCell(int)
  | SetDraggedPath(Path.t)
  | PickupWord(Word.t)
  | InsertWord(Path.t, int, Word.t)
  | InsertCell(int, Cell.t)
  | Delete(Path.t)
  | DeleteFocussedWord
  | UpdateWord(Path.t, Word.t => Word.t)
  | UpdateFocusedWord(Word.t => Word.t)
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

let update_trash = (f, {trash, _} as model: Model.t) => {
  ...model,
  trash: f(trash),
};

let num_words_expression = (block, cell_idx) =>
  Block.nth_cell(block, cell_idx)
  |> ((x: Cell.t) => x.expression)
  |> List.length;

let is_only_word_empty_expression = (block, path, cell_idx) =>
  num_words_expression(block, cell_idx) == 1
  && Path.get_word(path, block) == Some(Word.empty);

let is_path_to_cell: Path.t => bool = path => List.length(path) == 1;

let is_path_to_word: Path.t => bool = path => List.length(path) == 3;

let rec apply: (Model.t, t, unit, ~schedule_action: 'a) => Model.t =
  (model: Model.t, update: t, state: State.t, ~schedule_action) => {
    let model = update_drop_target(_ => NoTarget, model);
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
        print_endline(Sexplib.Sexp.to_string_hum(Path.sexp_of_t(path)));
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
              Block.update_expression(
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
      | AddWordToTrash(word, (x, y)) =>
        update_trash(trash => [TrashedWord(word, (x, y)), ...trash], model)
      | EmptyTrash => update_trash(_ => [], model)
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
                Block.update_expression(
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
              Block.update_expression(
                world,
                cell_idx,
                Cell.update_word(f, word_idx),
              )
            | _ => world
            },
          model,
        )
      };
    update_world(Interpreter.run_block, model);
  };

/*
 TODO:
 dragging on numbers changes instead of moves?
  */
