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

let exp_atom_class: Expression.atom => string =
  fun
  | Lit(_) => "exp-atom-lit"
  | Var(_) => "exp-atom-var"
  | Unbound(_) => "exp-atom-unbound"
  | Operator(_) => "exp-atom-operator"
  | Formless(_) => "exp-atom-formless";

let pat_atom_classes: option(Pattern.atom) => list(string) =
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

let expression_class: Expression.form => string =
  fun
  | Atom(_) => "expr-singleton"
  | App(_) => "expr-app"
  | Seq(_) => "expr-seq"
  | _ => "expr-unknown";

let pattern_class: option(Pattern.form) => string =
  fun
  | Some(Atom(_)) => "pat-singleton"
  | _ => "pat-unknown";

let core_word_view: (Model.pattern_display, Word.t) => t =
  (pattern_display, word) =>
    switch (pattern_display) {
    | Emoji => text(Word.emoji_of_default(word))
    | Name => text(word)
    };

let set_focus = (this_path, inject, _evt) =>
  stop(inject(SetFocus(SingleCell(this_path))));

let focus_word = (path: option(Path.t), i: int): option(Path.t) =>
  switch (path) {
  | Some(path) => Path.focus_word(path, i)
  | None => None
  };

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

let pat_atom_view =
    (
      {word, path: this_path, form, _} as ann_pat: AnnotatedBlock.annotated_word_pat,
      ~path: option(Path.t),
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
      Attr.on("dragstart", _ => stop(inject(Pickup(WordPat(ann_pat))))),
    ],
    [word_view],
  );
};

let val_atom_view =
    (
      {word, path: this_path, _}: AnnotatedBlock.annotated_word,
      ~path: option(Path.t),
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

let pattern_view =
    (
      {words, path: _, form, _}: AnnotatedBlock.annotated_pat,
      ~path: option(Path.t),
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
        pat_atom_view(word, ~path=focus_word(path, idx), ~inject, ~model),
      words,
    ),
  );
};

let value_view =
    (
      {words, path: _}: AnnotatedBlock.annotated_field,
      ~path: option(Path.t),
      ~inject,
    ) => {
  div(
    [Attr.classes(["value-view", atom_focus_class(path)])],
    List.mapi(
      (idx, word) =>
        val_atom_view(word, ~path=focus_word(path, idx), ~inject),
      words,
    ),
  );
};

let cell_sep_view =
    (~inject, ~model as {drop_target, carry, _}: Model.t, sep_idx) => {
  let this_target: Model.drop_target = CellSepatator(sep_idx);
  let is_drop_target =
    drop_target != NoTarget
    && drop_target == this_target
    && (
      switch (carry) {
      | WordExp({form: Lit(_) | Unbound(_), path, _})
          when Path.is_cell_idx((==)(sep_idx), path) =>
        true
      | Cell(_)
      | CellBrush(_) => true
      | _ => false
      }
    );
  div(
    [
      Attr.classes(
        ["cell-separator"] @ (is_drop_target ? ["active-drop-target"] : []),
      ),
      Attr.on_click(_ => stop(inject(InsertNewCell(sep_idx)))),
      Attr.on("drop", _ => stop(inject(DropOnCellSep(sep_idx)))),
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
      ~inject,
      ~model as {drop_target, carry, _}: Model.t,
      exp_path: Path.t,
      idx,
    ) => {
  let this_target: Model.drop_target =
    switch (exp_path) {
    | [Cell(Index(cell_idx, _)), Field(f), ..._] =>
      WordSeparator((cell_idx, f, idx))
    | _ => NoTarget
    };
  let is_drop_target =
    drop_target != NoTarget
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
      Attr.on_click(_ => stop(inject(InsertNewWord(exp_path, idx)))),
      Attr.on("drop", _ => stop(inject(DropOnWordSep(exp_path, idx)))),
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
      ~model,
      ~path: option(Path.t),
      {path: this_path, expression, pattern, value, _}: AnnotatedBlock.annotated_cell,
      idx,
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

let cells_view = (~inject, ~model, ~path: Path.t, cells) => {
  let focus = Path.focus_cell(path);
  let cell_views =
    List.mapi(
      (idx, cell) =>
        cell_view(~inject, ~model, ~path=focus(idx), cell, idx),
      cells,
    );
  let sep_views =
    List.init(List.length(cell_views) + 1, cell_sep_view(~inject, ~model));
  let views = Util.ListUtil.interleave(sep_views, cell_views);
  div(
    [Attr.class_("cells-view"), Attr.on("drop", _ => stop(Event.Ignore))],
    views,
  );
};

let tool_atom_view = (~inject, ~model: Model.t, word): t => {
  div(
    [
      //random_offset(word), //disabled for anim-test
      //Attr.on_click(_ => stop(inject(Animtest(true)))),
      Attr.classes(["atom", "toolbar-atom", "tool-anim-test"]),
      Attr.create("draggable", "true"),
      Attr.create(
        "style",
        model.animtest ? "transform: scale(110%)" : "transform: scale(100%)",
      ),
      Attr.on_mousedown(_ => Event.(Many([Stop_propagation]))),
      Attr.on_click(_ =>
        Event.(
          Many([
            stop(inject(UniFocus(UpdateWord(_ => word)))),
            stop(inject(Animtest(true))),
          ])
        )
      ),
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

let trash_view = (~inject, ~model as {trash, _}: Model.t) => {
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
      Attr.on("dragover", _evt => Event.Prevent_default),
      Attr.on("dragenter", _evt => Event.Prevent_default),
      ...Keyboard.handlers(~inject, model),
    ],
    [
      trash_panel(~inject),
      cell_control_panel(~inject),
      toolbar(~inject, ~model),
      title_view(~inject, ~model),
      cells_view(~inject, ~model, ~path, cells),
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
 */
