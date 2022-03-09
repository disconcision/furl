open Update;
open Core;
open ViewUtil;
open Virtual_dom.Vdom;
open Virtual_dom.Vdom.Node;

let atom_focus_class: option(Path.t) => string =
  path =>
    switch (path) {
    | None => "unfocussed"
    | Some([]) => "focussed"
    | Some(_) => "on-path"
    };

let set_focus = (this_path, inj, _evt) =>
  stop(inj(SetFocus(SingleCell(this_path))));

let focus_word = (path: option(Path.t), i: int): option(Path.t) =>
  switch (path) {
  | Some(path) => Path.focus_word(path, i)
  | None => None
  };

let core_word_view: (Model.pattern_display, Word.t) => t =
  (pattern_display, word) =>
    switch (pattern_display) {
    | Emoji => text(Word.emoji_of_default(word))
    | Name => text(word)
    };

let word_sep_view =
    (
      ~inj,
      ~model as {drop_target, carry, world, _}: Model.t,
      exp_path: Path.t,
      idx,
    ) => {
  let this_target: Model.drop_target =
    switch (exp_path) {
    | [Cell(Index(cell_idx, _)), Field(f), ..._] =>
      WordSeparator((cell_idx, f, idx))
    | _ => NoTarget
    };
  /*
   for separator at index idx, check if words at idx-1 and idx are empty
    */

  let is_drop_target =
    !Path.is_word_sep_touching_empty(exp_path, idx, world)
    && drop_target != NoTarget
    && drop_target == this_target
    && (
      switch (carry, Path.cell_idx(exp_path)) {
      | (WordPat({form: Some(Var(_)), path, _}), Some(exp_idx))
          when Path.is_cell_idx((>)(exp_idx), path) =>
        true
      | (WordExp(_) | WordBrush(_), _) => true
      | _ => false
      }
    );
  div(
    [
      Attr.classes(
        ["word-separator"] @ (is_drop_target ? ["active-drop-target"] : []),
      ),
      Attr.on_click(_ => stop(inj(InsertNewWord(exp_path, idx)))),
      Attr.on("drop", _ => stop(inj(DropOnWordSep(exp_path, idx)))),
      Attr.on("dragover", _ => Event.Prevent_default),
      Attr.on("dragenter", _ => prevent(inj(SetDropTarget(this_target)))),
      //Attr.on("dragleave", _evt => inj(SetDropTarget(NoTarget))),
    ],
    [text("·")],
  );
};
