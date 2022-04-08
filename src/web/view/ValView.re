open Update;
open Core;
open ViewUtil;
open CommonView;
open Virtual_dom.Vdom;
open Virtual_dom.Vdom.Node;

let val_atom_view =
    (
      {word, path: this_path, form, _}: AnnotatedBlock.annotated_word_val,
      ~path: option(Path.t),
      ~inj,
    )
    : t =>
  div(
    [
      random_offset(word.name),
      Attr.classes(
        ["atom", "value-atom", atom_focus_class(path)]
        @ (
          switch (form) {
          | Unknown(_) => ["value-unknown"]
          | _ => []
          }
        ),
      ),
      Attr.on_click(set_focus(this_path, inj)),
    ],
    [text(word.name)],
  );

let view =
    (
      {words, path: _, _}: AnnotatedBlock.annotated_val,
      ~path: option(Path.t),
      ~inj,
    ) =>
  div(
    [Attr.classes(["value-view", atom_focus_class(path)])],
    List.mapi(
      (idx, word) => val_atom_view(word, ~path=focus_word(path, idx), ~inj),
      words,
    ),
  );
