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

let rec intersperse = (sep, ls) =>
  switch (ls) {
  | []
  | [_] => ls
  | [hd, ...tl] => [hd] @ [sep] @ intersperse(sep, tl)
  };

let rec interleave = (xs, ys) =>
  switch (xs) {
  | [] => ys
  | [x, ...xs] => [x, ...interleave(ys, xs)]
  };

let divc = (cls, contents) => div([Attr.class_(cls)], contents);

let exp_atom_view =
    (
      {word, path: this_path}: FurlModel.annotated_word,
      ~path: option(FurlModel.path),
      ~inject,
    )
    : t => {
  let atom_class =
    switch (path) {
    | None => "unfocussed"
    | Some([]) => "focussed"
    | Some(_) => "on-path"
    };
  div(
    [
      Attr.classes(["atom", "expression-atom", atom_class]),
      Attr.on_click(_evt =>
        Event.(
          Many([
            Stop_propagation,
            inject(FurlUpdate.SetFocus(SingleCell(this_path))),
          ])
        )
      ),
      Attr.create("draggable", "true"),
      Attr.on("dragstart", _evt => {
        Event.(
          Many([
            Stop_propagation,
            inject(FurlUpdate.SetDraggedPath(this_path)),
            inject(FurlUpdate.PickupWord(word)),
          ])
        )
      }),
    ],
    [text(word)],
  );
};

let pat_atom_view =
    (
      {word: s, path: this_path}: FurlModel.annotated_word,
      ~path: option(FurlModel.path),
      ~inject,
    )
    : t => {
  let atom_class =
    switch (path) {
    | None => "unfocussed"
    | Some([]) => "focussed"
    | Some(_) => "on-path"
    };
  div(
    [
      Attr.classes(["atom", "pattern-atom", atom_class]),
      Attr.on_click(_evt =>
        Event.(
          Many([
            Stop_propagation,
            inject(FurlUpdate.SetFocus(SingleCell(this_path))),
          ])
        )
      ),
    ],
    [text(s)],
  );
};

let pattern_view =
    (
      {words, path: _}: FurlModel.annotated_field,
      ~path: option(FurlModel.path),
      ~inject,
    ) => {
  let get_focus = (i: int): option(FurlModel.path) =>
    switch (path) {
    | Some([Word(Index(idx, _)), ...subpath]) =>
      i == idx ? Some(subpath) : None
    | _ => None
    };
  let pattern_class =
    switch (path) {
    | None => "unfocussed"
    | Some([]) => "focussed"
    | Some(_) => "on-path"
    };
  div(
    [Attr.classes(["pattern-view", pattern_class])],
    List.mapi(
      (idx, word) => pat_atom_view(word, ~path=get_focus(idx), ~inject),
      words,
    ),
  );
};

// seperator functionality:
// clicking on a seperator inserts a new (placeholder) word
// action: InsertWord(ppath, int,"_") where int is _seperator_ index
// ppath is path to expression
// dropping on a seperator copies a dragged word at that place
// action: InsertWord(ppath, int,copied_word) where int is _seperator_ index
let sep_view = (inject, expression_path, model: FurlModel.t, idx) => {
  div(
    [
      Attr.classes(["word-separator"]),
      Attr.on_click(_evt =>
        Event.(
          Many([
            Stop_propagation,
            inject(FurlUpdate.InsertWord(expression_path, idx, "_")),
          ])
        )
      ),
      Attr.on("drop", _evt =>
        Event.(
          Many([
            Stop_propagation,
            inject(
              FurlUpdate.InsertWord(expression_path, idx, model.carried_word),
            ),
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
      {words, path: path_this}: FurlModel.annotated_field,
      ~path: option(FurlModel.path),
      ~inject,
      model,
    ) => {
  let get_focus = (i: int): option(FurlModel.path) =>
    switch (path) {
    | Some([Word(Index(idx, _)), ...subpath]) =>
      i == idx ? Some(subpath) : None
    | _ => None
    };
  let expression_class =
    switch (path) {
    | None => "unfocussed"
    | Some([]) => "focussed"
    | Some(_) => "on-path"
    };
  let word_views =
    List.mapi(
      (idx, word) => exp_atom_view(word, ~path=get_focus(idx), ~inject),
      words,
    );
  let sep_views =
    List.init(
      List.length(word_views) + 1,
      sep_view(inject, path_this, model),
    );
  let views = interleave(sep_views, word_views);
  div([Attr.classes(["expression-view", expression_class])], views);
};

let cell_view =
    (
      ~inject,
      {path: this_path, expression, pattern, _}: FurlModel.annotated_cell,
      ~path: option(FurlModel.path),
      idx,
      model: FurlModel.t,
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
  div(
    [
      Attr.classes(["cell-view", cell_class]),
      Attr.on_click(_evt =>
        inject(FurlUpdate.SetFocus(SingleCell(this_path)))
      ),
      Attr.create("draggable", "true"),
      Attr.on("dragstart", _evt =>
        Event.(
          Many([
            inject(FurlUpdate.SetDraggedPath(this_path)),
            inject(FurlUpdate.PickupCell(idx)),
          ])
        )
      ),
      Attr.on("drop", _evt =>
        inject(FurlUpdate.SwapCells(idx, model.carried_cell))
      ),
      Attr.on("dragover", _evt => {Event.Prevent_default}),
      Attr.on("dragenter", _evt => {Event.Prevent_default}),
    ],
    [
      pattern_view(pattern, ~path=pattern_path, ~inject),
      expression_view(expression, ~path=expression_path, ~inject, model),
    ],
  );
};

let title_view = ({dragged_path, _}: FurlModel.t, ~inject) =>
  div(
    [
      Attr.class_("title"),
      Attr.on("drop", _evt => {
        Event.(
          Many([
            Stop_propagation,
            inject(FurlUpdate.DeleteCell(dragged_path)),
          ])
        )
      }),
      Attr.on("dragover", _evt => {Event.Prevent_default}),
      Attr.on("dragenter", _evt => {Event.Prevent_default}),
    ],
    [text("furl 🗑️")],
  );

let view = (~inject, {world, focus, _} as model: FurlModel.t) => {
  let {path: _this_path, cells}: FurlModel.annotated_block =
    FurlModel.annotate_block(world);
  let SingleCell(path) = focus;
  //let is_focussed = path == [];
  let get_focus = (i: int): option(FurlModel.path) =>
    switch (path) {
    | [] => None
    | [Cell(Index(idx, _)), ...subpath] => i == idx ? Some(subpath) : None
    | _ => failwith("FurlView.view impossible")
    };
  let block_class =
    switch (path) {
    | [] => "focussed"
    | _ => "on-path"
    };
  div(
    [Attr.class_(block_class)]
    @ [Attr.id("root"), ...FurlKeyboard.handlers(~inject, model)],
    [title_view(model, ~inject)]
    @ List.mapi(
        (idx, cell) =>
          cell_view(~inject, cell, ~path=get_focus(idx), idx, model),
        cells,
      ),
  );
};
