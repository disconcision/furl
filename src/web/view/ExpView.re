open Update;
open Core;
open ViewUtil;
open CommonView;
open Virtual_dom.Vdom;
open Virtual_dom.Vdom.Node;

let exp_atom_class: Expression.atom => string =
  fun
  | Lit(_) => "exp-atom-lit"
  | Var(_) => "exp-atom-var"
  | Unbound(_) => "exp-atom-unbound"
  | Operator(_) => "exp-atom-operator"
  | Formless(_) => "exp-atom-formless";

let expression_class: Expression.form => string =
  fun
  | Atom(_) => "expr-singleton"
  | App(_) => "expr-app"
  | Seq(_) => "expr-seq"
  | _ => "expr-unknown";

let exp_atom_view =
    (
      {word, path: this_path, form, _} as ann_word: AnnotatedBlock.annotated_word_exp,
      ~path: option(Path.t),
      ~inject,
      ~model: Model.t,
    )
    : t => {
  let this_target: Model.drop_target =
    switch (this_path) {
    | [Cell(Index(cell_idx, _)), Field(f), Word(Index(word_idx, _)), ..._] =>
      Word((cell_idx, f, word_idx))
    | _ => NoTarget
    };
  let is_drop_target =
    model.drop_target != NoTarget
    && model.drop_target == this_target
    && (
      switch (model.carry, Path.cell_idx(this_path)) {
      | (WordPat({form: Some(Var(_)), path, _}), Some(exp_idx))
          when Path.is_cell_idx((>)(exp_idx), path) =>
        true
      | (WordExp(_) | WordBrush(_), _) => true
      | _ => false
      }
    );
  let binding_highlight_class =
    switch (form, model.focus) {
    | (Var(_, binding_path), SingleCell(focussed_path))
        when binding_path == focussed_path => [
        "binder-selected",
      ]
    | _ => []
    };
  let word_view =
    switch (form) {
    | Var(_) => core_word_view(model.pattern_display, word)
    | _ => text(word)
    };
  div(
    [
      random_offset(word),
      Attr.classes(
        [
          "atom",
          "expression-atom",
          exp_atom_class(form),
          atom_focus_class(path),
        ]
        @ binding_highlight_class
        @ (is_drop_target ? ["active-drop-target"] : []),
      ),
      Attr.on_click(set_focus(this_path, inject)),
      Attr.create("draggable", "true"),
      Attr.on("dragstart", _ => stop(inject(Pickup(WordExp(ann_word))))),
      Attr.on("dragenter", _ =>
        stop(prevent(inject(SetDropTarget(this_target))))
      ),
      Attr.on("dragend", _ => inject(SetDropTarget(NoTarget))),
      Attr.on("drop", _ => stop(inject(DropOnWord(this_path)))),
    ],
    [word_view],
  );
};

let view =
    (
      {words, path: path_this, form, _}: AnnotatedBlock.annotated_exp,
      ~path: option(Path.t),
      ~inject,
      ~model,
    ) => {
  let word_views =
    List.mapi(
      (idx, word) =>
        exp_atom_view(word, ~path=focus_word(path, idx), ~inject, ~model),
      words,
    );
  let sep_views =
    List.init(
      List.length(word_views) + 1,
      word_sep_view(~inject, ~model, path_this),
    );
  div(
    [
      Attr.classes([
        "expression-view",
        atom_focus_class(path),
        expression_class(form),
      ]),
    ],
    Util.ListUtil.interleave(sep_views, word_views),
  );
};
