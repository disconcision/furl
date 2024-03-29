open Update;
open Core;
open ViewUtil;
open CommonView;
open Virtual_dom.Vdom;
open Virtual_dom.Vdom.Node;

let title_view = (~model as _, ~inj as _) =>
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

let brush_view = (~inj, ~model as _: Model.t, word): t => {
  div(
    [
      random_offset(word),
      Attr.classes(["atom", "toolbar-atom"]),
      Attr.create("draggable", "true"),
      Attr.on_mousedown(_ => Event.(Many([Stop_propagation]))),
      Attr.on_click(_ => stop(inj(UniFocus(UpdateWordF(_ => word))))),
      Attr.on("dragstart", _ => stop(inj(Pickup(WordBrush(word))))),
      Attr.on("dragend", _ => inj(SetDropTarget(NoTarget))),
    ],
    [text(word)],
  );
};

let brushes_panel = (~inj, ~model): t =>
  div(
    [Attr.classes(["toolbar"])],
    List.map(
      brush_view(~inj, ~model),
      ["sum", "prod", "fact", "1337", "0", "1", "+", "*"],
    ),
  );

let trash_item_view = (~inj, trash_idx, item) => {
  let (item_view, x, y) =
    switch (item) {
    | Model.TrashedWord(word, (x, y)) => (text(word.name), x, y)
    | Model.TrashedCell(cell, (x, y)) =>
      switch (cell.pattern) {
      | [w, ..._] => (text(w.name ++ "..."), x, y)
      | _ => (text("lol"), x, y)
      }
    };
  div(
    [
      Attr.class_("trash-item"),
      Attr.create("draggable", "true"),
      Attr.on("dragstart", _ => stop(inj(PickupTrash(trash_idx)))),
      Attr.on("dragend", _ => inj(SetDropTarget(NoTarget))),
      Attr.string_property(
        "style",
        Printf.sprintf("position: absolute; top:%dpx; left: %dpx;", y, x),
      ),
    ],
    [item_view],
  );
};

let trash_view = (~inj, ~model as {trash, _}: Model.t) =>
  div([Attr.class_("trash")], List.mapi(trash_item_view(~inj), trash));

let toggle_panel = (name, icon, action, ~inj, is_off) =>
  div(
    [
      Attr.classes([name] @ (is_off ? ["panel-off"] : [])),
      Attr.on_click(_ => inj(action)),
    ],
    [text(icon)],
  );

let trash_panel = toggle_panel("trash-panel", "🗑", EmptyTrash);
let anim_control_panel =
  toggle_panel("anim-control-panel", "🎬", ToggleAnimations);

let cell_control_panel = (~inj, pattern_display) =>
  div(
    [
      Attr.classes(
        ["cell-control-panel"]
        @ (pattern_display == Model.Emoji ? ["panel-off"] : []),
      ),
      Attr.on_click(_ => inj(TogglePatternDisplay)),
    ],
    [text("P")],
  );

let view = (~inj, ~model: Model.t) => {
  let {cells, _}: AnnotatedBlock.t = AnnotatedBlock.mk(model.world);
  let SingleCell(path) = model.focus;
  let block_class =
    switch (path) {
    | [] => "focussed"
    | _ => "on-path"
    };
  let trash_carry = evt => inj(TrashCarry((evt##.clientX, evt##.clientY)));
  let focus_root = _ => inj(SetFocus(SingleCell([])));
  div(
    [Attr.class_(block_class)]
    @ [
      Attr.id("root"),
      Attr.on_click(focus_root),
      Attr.on("drop", trash_carry),
      Attr.on("dragover", _ => Event.Prevent_default),
      Attr.on("dragenter", _ => Event.Prevent_default),
      ...Keyboard.handlers(~inj, model),
    ],
    [
      title_view(~inj, ~model),
      trash_panel(~inj, model.trash == []),
      anim_control_panel(~inj, model.animations_off),
      cell_control_panel(~inj, model.pattern_display),
      brushes_panel(~inj, ~model),
      trash_view(~inj, ~model),
      BlockView.view(~inj, ~model, ~path, cells),
    ],
  );
};

/*
  TODO: styling

  - expressions (context-sensitive):
    - operators: adjacent-to-bad-word: opacity
  - expressions (fancy-semantic)
    - word: type-mismatch: color

  - patterns (context-sensitive)
    - word: var: starred: color (same as 2+uses)

  - values (context-free)
    - word: warning tag: add ?

  MISC: seperate ? on atoms into div for styling
 */
