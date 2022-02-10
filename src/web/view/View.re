open Virtual_dom.Vdom;
open Virtual_dom.Vdom.Node;

module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;

/*
 Attr.on("dragover", evt => {
          let container_rect =
            JsUtil.get_elem_by_id("root")##getBoundingClientRect;
          let (target_x, target_y) = (
            float_of_int(evt##.clientX),
            float_of_int(evt##.clientY),
          );
          let blee =
            Float.to_int(Float.round(target_y -. container_rect##.top) /. 30.);
          let blah =
            Float.to_int(
              Float.round(target_x -. container_rect##.left) /. 30.,
            );
          print_endline(string_of_int(evt##.clientX));
          print_endline(string_of_int(evt##.clientY));
          print_endline(string_of_int(blee));
          print_endline(string_of_int(blah));
          Event.Prevent_default;
        })
 */
let divc = (cls, contents) => div([Attr.class_(cls)], contents);
let atom_class: option(Core.Block.path) => string =
  path =>
    switch (path) {
    | None => "unfocussed"
    | Some([]) => "focussed"
    | Some(_) => "on-path"
    };

let set_focus = (this_path, inject, _evt) =>
  Event.(
    Many([
      Stop_propagation,
      inject(Update.SetFocus(SingleCell(this_path))),
    ])
  );

let exp_atom_view =
    (
      {word, path: this_path}: Core.Block.annotated_word,
      ~path: option(Core.Block.path),
      ~inject,
    )
    : t => {
  div(
    [
      Attr.classes(["atom", "expression-atom", atom_class(path)]),
      Attr.on_click(set_focus(this_path, inject)),
      Attr.create("draggable", "true"),
      Attr.on("dragstart", _evt => {
        Event.(
          Many([
            Stop_propagation,
            inject(Update.SetDraggedPath(this_path)),
            inject(Update.PickupWord(word)),
          ])
        )
      }),
    ],
    [text(word)],
  );
};

let pat_atom_view =
    (
      {word: s, path: this_path}: Core.Block.annotated_word,
      ~path: option(Core.Block.path),
      ~inject,
    )
    : t => {
  div(
    [
      Attr.classes(["atom", "pattern-atom", atom_class(path)]),
      Attr.on_click(set_focus(this_path, inject)),
    ],
    [text(s)],
  );
};

let val_atom_view =
    (
      {word: s, path: this_path}: Core.Block.annotated_word,
      ~path: option(Core.Block.path),
      ~inject,
    )
    : t => {
  div(
    [
      Attr.classes(["atom", "value-atom", atom_class(path)]),
      Attr.on_click(set_focus(this_path, inject)),
    ],
    [text(s)],
  );
};

let get_focus =
    (path: option(Core.Block.path), i: int): option(Core.Block.path) =>
  switch (path) {
  | Some([Word(Index(idx, _)), ...subpath]) =>
    i == idx ? Some(subpath) : None
  | _ => None
  };

let pattern_view =
    (
      {words, path: _}: Core.Block.annotated_field,
      ~path: option(Core.Block.path),
      ~inject,
    ) => {
  div(
    [Attr.classes(["pattern-view", atom_class(path)])],
    List.mapi(
      (idx, word) =>
        pat_atom_view(word, ~path=get_focus(path, idx), ~inject),
      words,
    ),
  );
};

let value_view =
    (
      {words, path: _}: Core.Block.annotated_field,
      ~path: option(Core.Block.path),
      ~inject,
    ) => {
  div(
    [Attr.classes(["value-view", atom_class(path)])],
    List.mapi(
      (idx, word) =>
        val_atom_view(word, ~path=get_focus(path, idx), ~inject),
      words,
    ),
  );
};

let word_sep_view = (inject, expression_path, model: Model.t, idx) => {
  div(
    [
      Attr.classes(["word-separator"]),
      Attr.on_click(_evt =>
        Event.(
          Many([
            Stop_propagation,
            inject(Update.InsertWord(expression_path, idx, "_")),
          ])
        )
      ),
      Attr.on("drop", _evt =>
        Event.(
          Many([
            Stop_propagation,
            inject(
              Update.InsertWord(expression_path, idx, model.carried_word),
            ),
            // need to be more careful with paths if want to delete (case where same cell)
            // if delete before insert, then its fine if you're moving word left.
            // but if moving right, need to decrement insert idx
            //inject(Update.Delete(model.dragged_path)),
          ])
        )
      ),
      Attr.on("dragover", _evt => {Event.Prevent_default}),
      Attr.on("dragenter", _evt => {Event.Prevent_default}),
    ],
    [text("_")] //TODO: nbsp?
  );
};

let expression_view =
    (
      {words, path: path_this}: Core.Block.annotated_field,
      ~path: option(Core.Block.path),
      ~inject,
      model,
    ) => {
  let word_views =
    List.mapi(
      (idx, word) =>
        exp_atom_view(word, ~path=get_focus(path, idx), ~inject),
      words,
    );
  let sep_views =
    List.init(
      List.length(word_views) + 1,
      word_sep_view(inject, path_this, model),
    );
  let views = Util.ListUtil.interleave(sep_views, word_views);
  div([Attr.classes(["expression-view", atom_class(path)])], views);
};

let cell_view =
    (
      ~inject,
      {path: this_path, expression, pattern, value, _}: Core.Block.annotated_cell,
      ~path: option(Core.Block.path),
      idx,
      model: Model.t,
    )
    : t => {
  let cell_class =
    switch (path) {
    | None => "unfocussed"
    | Some([]) => "focussed"
    | Some(_) => "on-path"
    };
  let pattern_path =
    switch (path) {
    | Some([Field(Pattern), ...ps]) => Some(ps)
    | _ => None
    };
  let expression_path =
    switch (path) {
    | Some([Field(Expression), ...ps]) => Some(ps)
    | _ => None
    };
  let value_path =
    switch (path) {
    | Some([Field(Value), ...ps]) => Some(ps)
    | _ => None
    };
  div(
    [
      Attr.classes(["cell-view", cell_class]),
      Attr.on_click(_evt => inject(Update.SetFocus(SingleCell(this_path)))),
      Attr.create("draggable", "true"),
      Attr.on("dragstart", _evt =>
        Event.(
          Many([
            inject(Update.SetDraggedPath(this_path)),
            inject(Update.PickupCell(idx)),
          ])
        )
      ),
      Attr.on("drop", _evt =>
        inject(Update.SwapCells(idx, model.carried_cell))
      ),
      Attr.on("dragover", _evt => {Event.Prevent_default}),
      Attr.on("dragenter", _evt => {Event.Prevent_default}),
    ],
    [
      pattern_view(pattern, ~path=pattern_path, ~inject),
      expression_view(expression, ~path=expression_path, ~inject, model),
      value_view(value, ~path=value_path, ~inject),
    ],
  );
};

let title_view = ({dragged_path, _}: Model.t, ~inject) =>
  div(
    [
      Attr.class_("title"),
      Attr.on("drop", _evt => {
        Event.(
          Many([Stop_propagation, inject(Update.Delete(dragged_path))])
        )
      }),
      Attr.on("dragover", _evt => {Event.Prevent_default}),
      Attr.on("dragenter", _evt => {Event.Prevent_default}),
    ],
    [text("furl 🗑️")],
  );

let view = (~inject, {world, focus, _} as model: Model.t) => {
  let {path: _this_path, cells}: Core.Block.annotated_block =
    Core.Block.annotate_block(world);
  let SingleCell(path) = focus;
  let get_focus = (i: int): option(Core.Block.path) =>
    switch (path) {
    | [] => None
    | [Cell(Index(idx, _)), ...subpath] => i == idx ? Some(subpath) : None
    | _ => failwith("Keyboard.view impossible")
    };
  let block_class =
    switch (path) {
    | [] => "focussed"
    | _ => "on-path"
    };
  div(
    [Attr.class_(block_class)]
    @ [Attr.id("root"), ...Keyboard.handlers(~inject, model)],
    [title_view(model, ~inject)]
    @ List.mapi(
        (idx, cell) =>
          cell_view(~inject, cell, ~path=get_focus(idx), idx, model),
        cells,
      ),
  );
};
