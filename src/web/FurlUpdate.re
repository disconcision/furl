open Sexplib.Std;

[@deriving sexp]
type t =
  | SwapCells(int, int)
  | PickupCell(int)
  | SetDraggedPath(FurlModel.path)
  | PickupWord(string)
  | InsertWord(FurlModel.path, int, string)
  | DeleteWord(FurlModel.path)
  | DeleteCell(FurlModel.path)
  | SetFocus(FurlModel.focus);

let list_swap = (u, v, xs) => {
  let e_u = List.nth(xs, u);
  let e_v = List.nth(xs, v);
  List.mapi((i, x) => i == u ? e_v : i == v ? e_u : x, xs);
};

let insert_at = (xs, i, n) =>
  if (i == List.length(xs)) {
    xs @ [n];
  } else {
    List.fold_left2(
      (acc, x, idx) => {acc @ (i == idx ? [n, x] : [x])},
      [],
      xs,
      List.init(List.length(xs), x => x),
    );
  };

let remove = (xs, i) =>
  List.fold_left2(
    (acc, x, idx) => {acc @ (i == idx ? [] : [x])},
    [],
    xs,
    List.init(List.length(xs), x => x),
  );

let update_furl = ({furl_model}: Model.t, f): Model.t => {
  furl_model: f(furl_model),
};

let update_focus = (f, {focus, _} as furl_model: FurlModel.t) => {
  ...furl_model,
  focus: f(focus),
};

let update_world = (f, {world, _} as furl_model: FurlModel.t) => {
  ...furl_model,
  world: f(world),
};

let update_carried_cell = (f, {carried_cell, _} as furl_model: FurlModel.t) => {
  ...furl_model,
  carried_cell: f(carried_cell),
};

let update_carried_word = (f, {carried_word, _} as furl_model: FurlModel.t) => {
  ...furl_model,
  carried_word: f(carried_word),
};

let update_dragged_path = (f, {dragged_path, _} as furl_model: FurlModel.t) => {
  ...furl_model,
  dragged_path: f(dragged_path),
};

let rec apply: (Model.t, t, unit, ~schedule_action: 'a) => Model.t =
  (
    {furl_model, _} as model: Model.t,
    update: t,
    state: State.t,
    ~schedule_action,
  ) =>
    switch (update) {
    | SetFocus(focus) => update_furl(model, update_focus(_ => focus))
    | SetDraggedPath(path) =>
      update_furl(model, update_dragged_path(_ => path))
    | PickupCell(idx) => update_furl(model, update_carried_cell(_ => idx))
    | PickupWord(word) => update_furl(model, update_carried_word(_ => word))
    | SwapCells(a, b) =>
      update_furl(model, update_world(world => list_swap(a, b, world)))
    | DeleteWord(path) =>
      switch (path) {
      | [
          Cell(Index(cell_idx, _)),
          Field(Expression),
          Word(Index(word_idx, _)),
          ..._,
        ] =>
        update_furl(
          model,
          update_world(world =>
            FurlModel.update_expression(world, cell_idx, words =>
              remove(words, word_idx)
            )
          ),
        )
      | _ => model
      }
    | DeleteCell(path) =>
      switch (path) {
      | [
          Cell(Index(cell_idx, _)),
          Field(Expression),
          Word(Index(word_idx, _)),
          ..._,
        ] =>
        update_furl(
          model,
          update_world(world =>
            FurlModel.update_expression(world, cell_idx, words =>
              remove(words, word_idx)
            )
          ),
        )
      | [Cell(Index(cell_idx, _)), ..._] =>
        update_furl(model, update_world(world => remove(world, cell_idx)))
      | _ => model
      }
    | InsertWord(ppath, sep_idx, new_word) =>
      let m =
        update_furl(
          model,
          update_world(world =>
            switch (ppath) {
            | [Cell(Index(cell_idx, _)), Field(Expression), ..._] =>
              FurlModel.update_expression(world, cell_idx, words =>
                insert_at(words, sep_idx, new_word)
              )
            | _ => furl_model.world
            }
          ),
        );
      apply(m, SetFocus(SingleCell(ppath)), state, ~schedule_action);
    };
