open Update;
open ViewUtil;
open Virtual_dom.Vdom;
open Virtual_dom.Vdom.Node;

let atom_focus_class: option(Core.Path.t) => string =
  path =>
    switch (path) {
    | None => "unfocussed"
    | Some([]) => "focussed"
    | Some(_) => "on-path"
    };

let exp_atom_class: Core.Expression.atom => string =
  fun
  | Lit(_) => "exp-atom-lit"
  | Var(_) => "exp-atom-var"
  | Unbound(_) => "exp-atom-unbound"
  | Operator(_) => "exp-atom-operator"
  | Formless(_) => "exp-atom-formless";

let pat_atom_classes: option(Core.Pattern.atom) => list(string) =
  fun
  | Some(Lit(_)) => ["pat-atom-lit"]
  | Some(Var(_, uses)) => {
      let uses =
        switch (uses) {
        | [] => ["unused"]
        | [_] => ["single-use"]
        | _ => ["many-uses"]
        };
      ["pat-atom-var"] @ uses;
    }
  | _ => ["pat-atom-formless"];

let expression_class: Core.Expression.form => string =
  fun
  | Atom(_) => "expr-singleton"
  | App(_) => "expr-app"
  | Seq(_) => "expr-seq"
  | _ => "expr-unknown";

let pattern_class: option(Core.Pattern.form) => string =
  fun
  | Some(Atom(_)) => "pat-singleton"
  | _ => "pat-unknown";

let core_word_view: (Model.pattern_display, Core.Word.t) => t =
  (pattern_display, word) =>
    switch (pattern_display) {
    | Emoji => text(Core.Word.emoji_of_default(word))
    | Name => text(word)
    };

let set_focus = (this_path, inject, _evt) =>
  stop(inject(SetFocus(SingleCell(this_path))));

let exp_atom_view =
    (
      {word, path: this_path, form, _}: Core.AnnotatedBlock.annotated_word_exp,
      ~path: option(Core.Path.t),
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
    switch (model.carry) {
    | Word(_)
    | WordBrush(_)
        when model.drop_target != NoTarget && model.drop_target == this_target =>
      true
    | _ => false
    };
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
      Attr.on("dragstart", _ => stop(inject(Pickup(Word(this_path))))),
      Attr.on("dragenter", _ =>
        stop(prevent(inject(SetDropTarget(this_target))))
      ),
      Attr.on("dragend", _ => inject(SetDropTarget(NoTarget))),
      Attr.on("drop", _ => stop(inject(DropReplaceWord(this_path)))),
    ],
    [word_view],
  );
};

let pat_atom_view =
    (
      {word, path: this_path, form, _}: Core.AnnotatedBlock.annotated_word_pat,
      ~path: option(Core.Path.t),
      ~inject,
      ~model: Model.t,
    )
    : t => {
  let binding_highlight_class =
    switch (form, model.focus) {
    | (Some(Var(_, uses)), SingleCell(focussed_path))
        when List.mem(focussed_path, uses) => [
        "use-selected",
      ]
    | _ => []
    };
  let word_view =
    switch (form) {
    | Some(Var(_)) => core_word_view(model.pattern_display, word)
    | _ => text(word)
    };
  div(
    [
      random_offset(word),
      Attr.classes(
        ["atom", "pattern-atom", atom_focus_class(path)]
        @ pat_atom_classes(form)
        @ binding_highlight_class,
      ),
      Attr.on_click(set_focus(this_path, inject)),
      Attr.create("draggable", "true"),
      Attr.on("dragstart", _ => stop(inject(Pickup(Word(this_path))))),
    ],
    [word_view],
  );
};

let val_atom_view =
    (
      {word, path: this_path, _}: Core.AnnotatedBlock.annotated_word,
      ~path: option(Core.Path.t),
      ~inject,
    )
    : t =>
  div(
    [
      random_offset(word),
      Attr.classes(["atom", "value-atom", atom_focus_class(path)]),
      Attr.on_click(set_focus(this_path, inject)),
    ],
    [text(word)],
  );

let get_focus = (path: option(Core.Path.t), i: int): option(Core.Path.t) =>
  switch (path) {
  | Some([Word(Index(idx, _)), ...subpath]) =>
    i == idx ? Some(subpath) : None
  | _ => None
  };

let pattern_view =
    (
      {words, path: _, form, _}: Core.AnnotatedBlock.annotated_pat,
      ~path: option(Core.Path.t),
      ~inject,
      ~model,
    ) => {
  div(
    [
      Attr.classes([
        "pattern-view",
        atom_focus_class(path),
        pattern_class(form),
      ]),
    ],
    List.mapi(
      (idx, word) =>
        pat_atom_view(word, ~path=get_focus(path, idx), ~inject, ~model),
      words,
    ),
  );
};

let value_view =
    (
      {words, path: _}: Core.AnnotatedBlock.annotated_field,
      ~path: option(Core.Path.t),
      ~inject,
    ) => {
  div(
    [Attr.classes(["value-view", atom_focus_class(path)])],
    List.mapi(
      (idx, word) =>
        val_atom_view(word, ~path=get_focus(path, idx), ~inject),
      words,
    ),
  );
};

let cell_sep_view =
    (~inject, {drop_target, carry, _} as _model: Model.t, cell_idx) => {
  let this_target: Model.drop_target = CellSepatator(cell_idx);
  let is_drop_target =
    switch (carry) {
    | Cell(_)
    | CellBrush(_) when drop_target != NoTarget && drop_target == this_target =>
      true
    | _ => false
    };
  div(
    [
      Attr.classes(
        ["cell-separator"] @ (is_drop_target ? ["active-drop-target"] : []),
      ),
      Attr.on_click(_ => stop(inject(InsertNewCell(cell_idx)))),
      Attr.on("drop", _ => stop(inject(DropReorderCell(cell_idx)))),
      Attr.on("dragover", _ => {Event.Prevent_default}),
      Attr.on("dragenter", _ =>
        prevent(inject(SetDropTarget(this_target)))
      ),
      //Attr.on("dragleave", _evt => inject(SetDropTarget(NoTarget))),
    ],
    [text("")],
  );
};

let word_sep_view =
    (
      inject,
      exp_path: Core.Path.t,
      {drop_target, carry, _} as _model: Model.t,
      idx,
    ) => {
  let this_target: Model.drop_target =
    switch (exp_path) {
    | [Cell(Index(cell_idx, _)), Field(f), ..._] =>
      WordSeparator((cell_idx, f, idx))
    | _ => NoTarget
    };
  let is_drop_target =
    switch (carry) {
    | Word(_)
    | WordBrush(_) when drop_target != NoTarget && drop_target == this_target =>
      true
    | _ => false
    };
  div(
    [
      Attr.classes(
        ["word-separator"] @ (is_drop_target ? ["active-drop-target"] : []),
      ),
      Attr.on_click(_ => stop(inject(InsertNewWord(exp_path, idx)))),
      Attr.on("drop", _ => stop(inject(DropInsertWord(exp_path, idx)))),
      Attr.on("dragover", _ => Event.Prevent_default),
      Attr.on("dragenter", _ =>
        prevent(inject(SetDropTarget(this_target)))
      ),
      //Attr.on("dragleave", _evt => inject(SetDropTarget(NoTarget))),
    ],
    [text("·")],
  );
};

let expression_view =
    (
      {words, path: path_this, form, _}: Core.AnnotatedBlock.annotated_exp,
      ~path: option(Core.Path.t),
      ~inject,
      ~model,
    ) => {
  let word_views =
    List.mapi(
      (idx, word) =>
        exp_atom_view(word, ~path=get_focus(path, idx), ~inject, ~model),
      words,
    );
  let sep_views =
    List.init(
      List.length(word_views) + 1,
      word_sep_view(inject, path_this, model),
    );
  let views = Util.ListUtil.interleave(sep_views, word_views);
  div(
    [
      Attr.classes([
        "expression-view",
        atom_focus_class(path),
        expression_class(form),
      ]),
    ],
    views,
  );
};

let cell_view =
    (
      ~inject,
      {path: this_path, expression, pattern, value, _}: Core.AnnotatedBlock.annotated_cell,
      ~path: option(Core.Path.t),
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
      random_skew(string_of_int(idx)),
      Attr.classes(["cell-view", cell_class]),
      Attr.on_click(set_focus(this_path, inject)),
      Attr.create("draggable", "true"),
      Attr.on("dragstart", _ => stop(inject(Pickup(Cell(this_path))))),
      Attr.on("dragend", _ => inject(SetDropTarget(NoTarget))),
      Attr.on("dragover", _evt => {Event.Prevent_default}),
      Attr.on("dragenter", _evt => {Event.Prevent_default}),
    ],
    [
      pattern_view(pattern, ~path=pattern_path, ~inject, ~model),
      expression_view(expression, ~path=expression_path, ~inject, ~model),
      value_view(value, ~path=value_path, ~inject),
    ],
  );
};

let title_view = (_model: Model.t, ~inject as _) =>
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

let cells_view = (~inject, ~model, path: Core.Path.t, cells) => {
  let get_focus = (i: int): option(Core.Path.t) =>
    switch (path) {
    | [] => None
    | [Cell(Index(idx, _)), ...subpath] => i == idx ? Some(subpath) : None
    | _ => failwith("View.cells_view impossible")
    };
  let cell_views =
    List.mapi(
      (idx, cell) =>
        cell_view(~inject, cell, ~path=get_focus(idx), idx, model),
      cells,
    );
  let sep_views =
    List.init(List.length(cell_views) + 1, cell_sep_view(~inject, model));
  let views = Util.ListUtil.interleave(sep_views, cell_views);
  div(
    [Attr.class_("cells-view"), Attr.on("drop", _ => stop(Event.Ignore))],
    views,
  );
};

let tool_atom_view = (~inject, word): t => {
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

let toolbar = (~inject): t =>
  div(
    [Attr.class_("toolbar")],
    List.map(
      tool_atom_view(~inject),
      ["fact", "sum", "prod", "+", "*", "0", "1", "1337"],
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

let trash_view = (~inject, {trash, _}: Model.t) => {
  div([Attr.class_("trash")], List.mapi(trash_item_view(~inject), trash));
};

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

let view = (~inject, {world, focus, _} as model: Model.t) => {
  let {cells, _}: Core.AnnotatedBlock.annotated_block =
    Core.AnnotatedBlock.mk(world);
  let SingleCell(path) = focus;
  let block_class =
    switch (path) {
    | [] => "focussed"
    | _ => "on-path"
    };
  let trash_carry = evt =>
    inject(AddCarryToTrash((evt##.clientX, evt##.clientY)));
  let focus_root = _ => inject(SetFocus(SingleCell([])));
  div(
    [Attr.class_(block_class)]
    @ [
      Attr.id("root"),
      Attr.on_click(focus_root),
      Attr.on("drop", trash_carry),
      Attr.on("dragover", _evt => Event.Prevent_default),
      Attr.on("dragenter", _evt => Event.Prevent_default),
      ...Keyboard.handlers(~inject, model),
    ],
    [
      trash_panel(~inject),
      cell_control_panel(~inject),
      toolbar(~inject),
      title_view(model, ~inject),
      cells_view(~inject, ~model, path, cells),
      trash_view(model, ~inject),
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
 */
