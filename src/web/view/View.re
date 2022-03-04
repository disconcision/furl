open Virtual_dom.Vdom;
open Virtual_dom.Vdom.Node;

module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;

let hash_of_string = str =>
  List.fold_left(
    (acc, c) => acc + int_of_char(c),
    0,
    List.of_seq(String.to_seq(str)),
  );

let random_offset = (~bound_x=5, ~bound_y=8, seed_str) => {
  Random.init(hash_of_string(seed_str));
  let (x, y) = (
    Random.int(bound_x) - bound_x / 2,
    Random.int(bound_y) - bound_y / 2,
  );
  Attr.string_property(
    "style",
    "position: relative; left: "
    ++ string_of_int(x)
    ++ "px; top: "
    ++ string_of_int(y)
    ++ "px;",
  );
};

let random_skew = (~bound_x=32, ~bound_y=1.2, seed_str) => {
  Random.init(hash_of_string(seed_str));
  let (x, y) = (
    Random.int(bound_x) - bound_x / 2,
    Random.float(bound_y) -. bound_y /. 2.,
  );
  Attr.string_property(
    "style",
    Printf.sprintf("transform: SkewY(%fdeg) SkewX(%ddeg);", y, x),
  );
};

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
  | Unknown => "expr-unknown";

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
  Event.(
    Many([
      Stop_propagation,
      inject(Update.SetFocus(SingleCell(this_path))),
    ])
  );

let exp_atom_view =
    (
      {word, path: this_path, form, _}: Core.AnnotatedBlock.annotated_word_exp,
      ~path: option(Core.Path.t),
      ~inject,
      ~model: Model.t,
    )
    : t => {
  /*
   let this_drop_target: Model.drop_target =
     switch (this_path) {
     | [Cell(Index(cell_idx, _)), Field(f), Word(Index(idx, _)), ..._] =>
       WordSeparator((cell_idx, f, idx))
     | _ => NoTarget
     };
   let is_drop_target =
     model.drop_target != NoTarget && model.drop_target == this_drop_target;
     */
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
          atom_focus_class(path) /*@ (is_drop_target ? ["active-drop-target"] : [])*/,
        ]
        @ binding_highlight_class,
      ),
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
      Attr.on("drop", _evt =>
        Event.(
          Many([
            Stop_propagation,
            inject(
              Update.UpdateWord(
                this_path,
                //_word => model.carried_word,
                word => word == Core.Word.empty ? model.carried_word : word,
              ),
            ),
          ])
        )
      ),
      /*
       Attr.on("dragenter", _evt => {
         Event.Many([
           Event.Prevent_default,
           inject(SetDropTarget(this_drop_target)),
         ])
       }),
       Attr.on("dragleave", _evt => {
         Event.Many([Event.Prevent_default, inject(SetDropTarget(NoTarget))])
       }),
       */
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

let word_sep_view =
    (
      inject,
      expression_path: Core.Path.t,
      {drop_target, _} as model: Model.t,
      idx,
    ) => {
  let this_drop_target: Model.drop_target =
    switch (expression_path) {
    | [Cell(Index(cell_idx, _)), Field(f), ..._] =>
      WordSeparator((cell_idx, f, idx))
    | _ => NoTarget
    };
  let is_drop_target =
    drop_target != NoTarget && drop_target == this_drop_target;
  div(
    [
      Attr.classes(
        ["word-separator"] @ (is_drop_target ? ["active-drop-target"] : []),
      ),
      Attr.on_click(_evt =>
        Event.(
          Many([
            Stop_propagation,
            inject(Update.InsertWord(expression_path, idx, Core.Word.empty)),
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
      Attr.on("dragenter", _evt => {
        Event.Many([
          Event.Prevent_default,
          inject(SetDropTarget(this_drop_target)),
        ])
      }),
      Attr.on("dragleave", _evt => {
        Event.Many([Event.Prevent_default, inject(SetDropTarget(NoTarget))])
      }),
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

let cell_sep_view = (~inject, model: Model.t, cell_idx) => {
  div(
    [
      Attr.classes(["cell-separator"]),
      Attr.on_click(_evt =>
        Event.(
          Many([
            Stop_propagation,
            inject(Update.InsertCell(cell_idx, Core.Cell.init())),
          ])
        )
      ),
      /*
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
       ),*/
      Attr.on("drop", _evt =>
        Event.(
          Many([
            Stop_propagation,
            inject(Update.SwapCells(cell_idx - 1, model.carried_cell)),
          ])
        )
      ),
      Attr.on("dragover", _evt => {Event.Prevent_default}),
      Attr.on("dragenter", _evt => {Event.Prevent_default}),
    ],
    [text("")],
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
      Attr.on("dragstart", _evt =>
        Event.(
          Many([
            inject(Update.SetDraggedPath(this_path)),
            inject(Update.PickupCell(idx)),
          ])
        )
      ),
      Attr.on("drop", _evt =>
        Event.(
          Many([
            Stop_propagation,
            inject(Update.SwapCells(idx, model.carried_cell)),
          ])
        )
      ),
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

let delete = (~inject, path, _evt) =>
  Event.(Many([Stop_propagation, inject(Update.Delete(path))]));

let title_view = ({dragged_path, _}: Model.t, ~inject) =>
  div(
    [
      Attr.class_("title"),
      Attr.on("drop", delete(~inject, dragged_path)),
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
    | _ => failwith("Keyboard.view impossible")
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
  div([Attr.class_("cells-view")], views);
};

let tool_atom_view = (~inject, word): t => {
  div(
    [
      random_offset(word),
      Attr.classes(["atom", "toolbar-atom"]),
      Attr.create("draggable", "true"),
      Attr.on("dragstart", _evt => {
        Event.(
          Many([
            Stop_propagation,
            inject(Update.SetDraggedPath([])),
            inject(Update.PickupWord(word)),
          ])
        )
      }),
    ],
    [text(word)],
  );
};

let toolbar = (~inject): t =>
  div(
    [Attr.class_("toolbar")],
    List.map(
      tool_atom_view(~inject),
      ["fact", "add", "mult", "+", "*", "0", "1", "1337"],
    ),
  );

let root_delete = (~inject, path, model: Model.t, evt) =>
  Event.Many(
    (
      switch (Core.Path.get_word(path, model.world)) {
      | None => []
      | Some(word) => [
          inject(
            Update.AddWordToTrash(word, (evt##.clientX, evt##.clientY)),
          ),
        ]
      }
    )
    @ [delete(~inject, path, evt)],
  );

let trash_item_view = (~inject, item) => {
  switch (item) {
  | Model.TrashedWord(word, (x, y)) =>
    div(
      [
        Attr.class_("trash-item"),
        Attr.create("draggable", "true"),
        Attr.on("dragstart", _evt => {
          Event.(
            Many([
              Stop_propagation,
              inject(Update.SetDraggedPath([])),
              inject(Update.PickupWord(word)),
            ])
          )
        }),
        Attr.string_property(
          "style",
          Printf.sprintf("position: absolute; top:%dpx; left: %dpx;", y, x),
        ),
      ],
      [text(word)],
    )
  };
};

let trash_view = (~inject, {trash, _}: Model.t) => {
  div([Attr.class_("trash")], List.map(trash_item_view(~inject), trash));
};

let trash_panel = (~inject) =>
  div(
    [
      Attr.class_("trash-panel"),
      Attr.on_click(_ => inject(Update.EmptyTrash)),
    ],
    [text("🗑️")],
  );

let cell_control_panel = (~inject) =>
  div(
    [
      Attr.class_("cell-control-panel"),
      Attr.on_click(_ => inject(Update.TogglePatternDisplay)),
    ],
    [text("P")],
  );

let view = (~inject, {world, focus, _} as model: Model.t) => {
  let {path: _this_path, cells}: Core.AnnotatedBlock.annotated_block =
    Core.AnnotatedBlock.mk(world);
  let SingleCell(path) = focus;
  let block_class =
    switch (path) {
    | [] => "focussed"
    | _ => "on-path"
    };
  div(
    [Attr.class_(block_class)]
    @ [
      Attr.id("root"),
      Attr.on_click(_ => inject(Update.SetFocus(SingleCell([])))),
      Attr.on("drop", root_delete(~inject, model.dragged_path, model)),
      Attr.on("dragover", _evt => {Event.Prevent_default}),
      Attr.on("dragenter", _evt => {Event.Prevent_default}),
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
