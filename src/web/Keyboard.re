open Virtual_dom.Vdom;
open Core;

let remove_char = str => String.sub(str, 0, String.length(str) - 1);

let is_prev_word_operator = (block, path: Path.t) =>
  switch (Core.Path.prev_word(block, path)) {
  | Some(op) when Expression.is_operator(op) => true
  | _ => false
  };

let handlers = (~inject: Update.t => Event.t, model: Model.t) => [
  Attr.on_keypress(_evt => Event.Prevent_default),
  Attr.on_keyup(_evt => Event.Many([])),
  /*
   Event.Many(
     switch (JsUtil.get_key(evt), zipper) {
     | ("Shift", (Selecting(_, [], _), _)) => [
         inject(Update.escape()),
         Event.Prevent_default,
       ]
     | _ => []
     },
   )*/
  Attr.on_keydown(evt => {
    let key = JsUtil.get_key(evt);
    let held = m => JsUtil.held(m, evt);
    let SingleCell(current_path) = model.focus;
    let update_focus = f => [
      Update.SetFocus(SingleCell(f(model.world, current_path))),
    ];
    let updates: list(Update.t) =
      if (!held(Ctrl) && !held(Alt) && !held(Meta)) {
        print_endline("key pressed:");
        print_endline(key);
        switch (key) {
        | "F3" => [PrintAnnotatedBlock]
        | "Shift" => []
        | "ArrowRight" => update_focus(Core.Path.next_word_path)
        | "ArrowLeft" => update_focus(Core.Path.prev_word_path)
        | "ArrowUp" => update_focus(Core.Path.up_word_path)
        | "ArrowDown" => update_focus(Core.Path.down_word_path)
        | "Enter" =>
          switch (current_path) {
          | [Cell(Index(cell_idx, _)), ..._] => [
              Update.InsertCell(cell_idx + 1, Core.Cell.init()),
            ]
          | _ => []
          }
        | " " =>
          switch (current_path) {
          | [_, _, Word(Index(n, _)), ..._]
              when
                Path.get_word(current_path, model.world)
                != Some(Core.Word.empty) => [
              InsertWord(current_path, n + 1, Core.Word.empty),
            ]
          | _ => []
          }
        | op when Core.Expression.is_operator(op) =>
          switch (current_path, Path.get_word(current_path, model.world)) {
          | (_, Some(op1)) when Core.Expression.is_operator(op1) => []
          | ([_, _, Word(Index(n, _)), ..._], _) => [
              InsertWord(current_path, n + 1, op),
              InsertWord(current_path, n + 2, Core.Word.empty),
            ]
          | _ => []
          }
        | "Delete" => [DeleteFocussedWord]
        | "Backspace" =>
          let words = Path.get_words(current_path, model.world);
          //TODO: if cell selected, select last word
          switch (words) {
          | Some([x]) when x == Core.Word.empty =>
            // if only empty word, delete cell
            switch (current_path) {
            | [Cell(Index(0, l)), ..._] => [
                Delete([Cell(Index(0, l))]),
                SetFocus(SingleCell([Cell(Index(0, 666))])),
              ]
            | [Cell(Index(cell_idx, l)), ..._] => [
                Delete([Cell(Index(cell_idx, l))]),
                SetFocus(SingleCell([Cell(Index(cell_idx - 1, 666))])),
              ]
            | _ => []
            }
          | _ =>
            /* Operators cannot be directly backspaced; if we try to
               backspace an empty word after an operator, we'll delete
               the operator. */
            switch (Path.get_word(current_path, model.world)) {
            | Some(word) when Core.Expression.is_operator(word) => []
            | Some(word)
                when
                  word == Core.Word.empty
                  && is_prev_word_operator(model.world, current_path) => [
                Update.DeleteFocussedWord,
                Update.SetFocus(
                  SingleCell(Core.Path.decr_word(current_path)),
                ),
                Update.DeleteFocussedWord,
                Update.SetFocus(
                  SingleCell(
                    current_path |> Core.Path.decr_word |> Core.Path.decr_word,
                  ),
                ),
              ]
            | Some(word) when word == Core.Word.empty => [
                Update.DeleteFocussedWord,
                Update.SetFocus(
                  SingleCell(Core.Path.decr_word(current_path)),
                ),
              ]
            | _ => [
                Update.UpdateFocusedWord(
                  str =>
                    String.length(str) == 1
                      ? Core.Word.empty : remove_char(str),
                ),
              ]
            }
          };
        | x => [
            Update.UpdateFocusedWord(
              str => str == Core.Word.empty ? x : str ++ x,
            ),
          ]
        };
      } else if (! Os.is_mac^ && held(Ctrl) && !held(Alt) && !held(Meta)) {
        switch (key) {
        | "z" => held(Shift) ? [] : []
        | _ => []
        };
      } else if (Os.is_mac^ && held(Meta) && !held(Alt) && !held(Ctrl)) {
        switch (key) {
        | "z" => held(Shift) ? [] : []
        | _ => []
        };
      } else {
        [];
      };
    switch (updates) {
    | [] => Event.Many([])
    | [_, ..._] =>
      Event.(
        Many([
          Prevent_default,
          Stop_propagation,
          ...List.map(inject, updates),
        ])
      )
    };
  }),
];
