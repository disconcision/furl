open Update;
open Core;
open ViewUtil;
open CommonView;
open Virtual_dom.Vdom;
open Virtual_dom.Vdom.Node;

let is_cell_sep_drop_target = (carry: Model.carry, sep_idx) =>
  switch (carry) {
  | WordExp({form: Lit(_) | Unbound(_), path, _})
      when Path.is_cell_idx((==)(sep_idx), path) =>
    true
  | Cell(_)
  | CellBrush(_) => true
  | _ => false
  };

let drop_target_class =
    (~model as {drop_target, carry, _}: Model.t, this_target, sep_idx) => {
  let is_drop_target =
    drop_target != NoTarget
    && drop_target == this_target
    && is_cell_sep_drop_target(carry, sep_idx);
  is_drop_target ? ["active-drop-target"] : [];
};

let cell_sep_view = (~inj, ~model: Model.t, sep_idx) => {
  let this_target: Model.drop_target = CellSepatator(sep_idx);
  div(
    [
      Attr.classes(
        ["cell-separator"] @ drop_target_class(~model, this_target, sep_idx),
      ),
      Attr.on_click(_ => stop(inj(InsertNewCell(sep_idx)))),
      Attr.on("drop", _ => stop(inj(DropOnCellSep(sep_idx)))),
      Attr.on("dragover", _ => {Event.Prevent_default}),
      Attr.on("dragenter", _ => prevent(inj(SetDropTarget(this_target)))),
      //Attr.on("dragleave", _evt => inj(SetDropTarget(NoTarget))),
    ],
    [text("")],
  );
};

let cell_focus_class = (path: option(Path.t)) =>
  switch (path) {
  | None => "unfocussed"
  | Some([]) => "focussed"
  | Some(_) => "on-path"
  };

let cell_view =
    (
      ~inj,
      ~model,
      ~path: option(Path.t),
      {path: this_path, expression, pattern, value, uid, _}: AnnotatedBlock.annotated_cell,
      idx,
    )
    : t => {
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
      random_skew(string_of_int(idx)),
      Attr.id(string_of_int(uid)),
      Attr.classes(["cell-view", cell_focus_class(path)]),
      Attr.on_click(set_focus(this_path, inj)),
      Attr.create("draggable", "true"),
      /*
        not sure if good idea... trying bcuz accidental drops on cell
        when trying to hit sep...
       */
      Attr.on("drop", _ => stop(inj(DropOnCellSep(idx + 1)))),
      Attr.on("dragstart", _ => stop(inj(Pickup(Cell(this_path))))),
      Attr.on("dragend", _ => inj(SetDropTarget(NoTarget))),
      Attr.on("dragover", _evt => {Event.Prevent_default}),
      Attr.on("dragenter", _evt => {Event.Prevent_default}),
    ],
    [
      PatView.view(pattern, ~path=pattern_path, ~inj, ~model),
      ExpView.view(expression, ~path=expression_path, ~inj, ~model),
      ValView.view(value, ~path=value_path, ~inj),
    ],
  );
};

let view = (~inj, ~model, ~path: Path.t, cells) => {
  let focus = Path.focus_cell(path);
  let cell_views =
    List.mapi(
      (idx, cell) => cell_view(~inj, ~model, ~path=focus(idx), cell, idx),
      cells,
    );
  let sep_views =
    List.init(List.length(cell_views) + 1, cell_sep_view(~inj, ~model));
  let views = Util.ListUtil.interleave(sep_views, cell_views);
  div(
    [Attr.class_("cells-view"), Attr.on("drop", _ => stop(Event.Ignore))],
    views,
  );
};
