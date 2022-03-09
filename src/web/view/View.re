open Update;
open Core;
open ViewUtil;
open CommonView;
open Virtual_dom.Vdom;
open Virtual_dom.Vdom.Node;

let title_view = (~model as _, ~inject as _) =>
  div(
    [
      Attr.class_("title"),
      Attr.on("dragover", _evt => {Event.Prevent_default}),
      Attr.on("dragenter", _evt => {Event.Prevent_default}),
    ],
    [
      divc("title-f", [text("f")]),
      divc("title-u", [text("u")]),
      divc("title-r", [text("r")]),
      divc("title-l", [text("l")]),
    ],
  );

let tool_atom_view = (~inject, ~model as _: Model.t, word): t => {
  div(
    [
      random_offset(word),
      Attr.classes(["atom", "toolbar-atom"]),
      Attr.create("draggable", "true"),
      Attr.on_mousedown(_ => Event.(Many([Stop_propagation]))),
      Attr.on_click(_ => stop(inject(UniFocus(UpdateWord(_ => word))))),
      Attr.on("dragstart", _ => stop(inject(Pickup(WordBrush(word))))),
      Attr.on("dragend", _ => inject(SetDropTarget(NoTarget))),
    ],
    [text(word)],
  );
};

let toolbar = (~inject, ~model): t =>
  div(
    [Attr.classes(["toolbar"])],
    List.map(
      tool_atom_view(~inject, ~model),
      ["sum", "prod", "fact", "1337", "0", "1", "+", "*"],
    ),
  );

let trash_item_view = (~inject, trash_idx, item) => {
  let (item_view, x, y) =
    switch (item) {
    | Model.TrashedWord(word, (x, y)) => (text(word), x, y)
    | Model.TrashedCell(cell, (x, y)) =>
      switch (cell.pattern) {
      | [w, ..._] => (text(w ++ "..."), x, y)
      | _ => (text("lol"), x, y)
      }
    };
  div(
    [
      Attr.class_("trash-item"),
      Attr.create("draggable", "true"),
      Attr.on("dragstart", _ => stop(inject(PickupTrash(trash_idx)))),
      Attr.on("dragend", _ => inject(SetDropTarget(NoTarget))),
      Attr.string_property(
        "style",
        Printf.sprintf("position: absolute; top:%dpx; left: %dpx;", y, x),
      ),
    ],
    [item_view],
  );
};

let trash_view = (~inject, ~model as {trash, _}: Model.t) =>
  div([Attr.class_("trash")], List.mapi(trash_item_view(~inject), trash));

let trash_panel = (~inject) =>
  div(
    [Attr.class_("trash-panel"), Attr.on_click(_ => inject(EmptyTrash))],
    [text("🗑️")],
  );

let cell_control_panel = (~inject) =>
  div(
    [
      Attr.class_("cell-control-panel"),
      Attr.on_click(_ => inject(TogglePatternDisplay)),
    ],
    [text("P")],
  );

let view = (~inject, ~model: Model.t) => {
  let {cells, _}: AnnotatedBlock.annotated_block =
    AnnotatedBlock.mk(model.world);
  let SingleCell(path) = model.focus;
  let block_class =
    switch (path) {
    | [] => "focussed"
    | _ => "on-path"
    };
  let trash_carry = evt =>
    inject(TrashCarry((evt##.clientX, evt##.clientY)));
  let focus_root = _ => inject(SetFocus(SingleCell([])));
  div(
    [Attr.class_(block_class)]
    @ [
      Attr.id("root"),
      Attr.on_click(focus_root),
      Attr.on("drop", trash_carry),
      Attr.on("dragover", _ => Event.Prevent_default),
      Attr.on("dragenter", _ => Event.Prevent_default),
      ...Keyboard.handlers(~inject, model),
    ],
    [
      trash_panel(~inject),
      cell_control_panel(~inject),
      toolbar(~inject, ~model),
      title_view(~inject, ~model),
      BlockView.view(~inject, ~model, ~path, cells),
      trash_view(~inject, ~model),
    ],
  );
};

/*
  TODO: styling

  - expressions (context-free):
    - application: background color
    DONE - application: first word: color
    DONE - operators: background color, color
    DONE - word: invalid: color, box, add '?'
  - expressions (context-sensitive):
    DONE - word: unbound: color, box, add '?'
    - operators: adjacent-to-bad-word: opacity
  - expressions (fancy-semantic)
    - word: type-mismatch: color

  - patterns (context-sensitive)
    DONE - word: invalid: color, box, add '?'
    DONE - word: var: {unused, 1-use, 2+uses}: color
    - word: var: starred: color (same as 2+uses)

  - values (context-free)
    - word: warning tag: add ?

  MISC: seperate ? on atoms into div for styling
 */
