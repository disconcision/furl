open Update;
open Core;
open ViewUtil;
open CommonView;
open Virtual_dom.Vdom;
open Virtual_dom.Vdom.Node;

let pattern_class: option(Pattern.t) => string =
  fun
  | Some(Atom(_)) => "pat-singleton"
  | _ => "pat-unknown";

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

let use_highlight_classes = (form: option(Pattern.atom), focus: Model.focus) =>
  switch (form, focus) {
  | (Some(Var(_, uses)), SingleCell(focussed_path))
      when List.mem(focussed_path, uses) => [
      "use-selected",
    ]
  | _ => []
  };

let pat_word_view =
    (~pattern_display, {word, form, _}: AnnotatedBlock.annotated_word_pat) =>
  switch (form) {
  | Some(Var(_)) => core_word_view(pattern_display, word)
  | _ => text(word.name)
  };

let pat_atom_view =
    (
      {word, path: this_path, form, _} as ann_pat: AnnotatedBlock.annotated_word_pat,
      ~path: option(Path.t),
      ~inj,
      ~model as {pattern_display, focus, _}: Model.t,
    )
    : t => {
  div(
    [
      random_offset(word.name),
      Attr.classes(
        ["atom", "pattern-atom", atom_focus_class(path)]
        @ pat_atom_classes(form)
        @ use_highlight_classes(form, focus),
      ),
      Attr.on_click(set_focus(this_path, inj)),
      Attr.create("draggable", "true"),
      Attr.on("dragstart", _ => stop(inj(Pickup(WordPat(ann_pat))))),
    ],
    [pat_word_view(~pattern_display, ann_pat)],
  );
};

let view =
    (
      {words, form, _}: AnnotatedBlock.annotated_pat,
      ~path: option(Path.t),
      ~inj,
      ~model,
    ) =>
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
        pat_atom_view(word, ~path=focus_word(path, idx), ~inj, ~model),
      words,
    ),
  );
