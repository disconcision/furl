open Update;
open Core;
open ViewUtil;
open CommonView;
open Virtual_dom.Vdom;
open Virtual_dom.Vdom.Node;

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

let view =
    (
      {words, path: _}: AnnotatedBlock.annotated_field,
      ~path: option(Path.t),
      ~inject,
    ) =>
  div(
    [Attr.classes(["value-view", atom_focus_class(path)])],
    List.mapi(
      (idx, word) =>
        val_atom_view(word, ~path=focus_word(path, idx), ~inject),
      words,
    ),
  );
