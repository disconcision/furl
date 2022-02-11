open Virtual_dom.Vdom;
open Core;

let remove_char = str => String.sub(str, 0, String.length(str) - 1);

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
    let updates: list(Update.t) =
      if (!held(Ctrl) && !held(Alt) && !held(Meta)) {
        print_endline("key pressed:");
        print_endline(key);
        switch (key) {
        // TODO: betterize this garbagio
        | "ArrowRight"
        | "ArrowLeft"
        | "ArrowUp"
        | "ArrowDown"
        | "Shift" => []
        | "Enter" =>
          let SingleCell(path) = model.focus;
          switch (path) {
          | [Cell(Index(cell_idx, _)), ..._] => [
              Update.InsertCell(cell_idx + 1, Core.Block.init_cell()),
            ]
          | _ => []
          };
        | " " =>
          let SingleCell(path) = model.focus;
          switch (path) {
          | [_, _, Word(Index(n, _)), ..._] => [
              InsertWord(path, n + 1, Core.Block.empty_word),
            ]
          | _ => []
          };
        | "Delete" => [DeleteFocussedWord]
        | "Backspace" =>
          let SingleCell(path) = model.focus;
          let words = Block.get_words_path(path, model.world);
          //TODO: if cell selected, select last word
          switch (words) {
          | Some([x]) when x == Core.Block.empty_word =>
            // if only empty word, delete cell
            switch (path) {
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
            switch (Block.get_word_path(path, model.world)) {
            | Some(s) when s == Core.Block.empty_word =>
              let new_path =
                switch (path) {
                | [c, _, Word(Index(0, _)), ..._] => [c]
                | [c, f, Word(Index(n, k)), ...ps] => [
                    c,
                    f,
                    Word(Index(n - 1, k)),
                    ...ps,
                  ]
                | _ => path
                };
              // TODO: move to previous word if any
              [
                Update.DeleteFocussedWord,
                Update.SetFocus(SingleCell(new_path)),
              ];
            | _ => [
                Update.UpdateFocusedWord(
                  str =>
                    String.length(str) == 1
                      ? Core.Block.empty_word : remove_char(str),
                ),
              ]
            }
          };

        | x => [
            Update.UpdateFocusedWord(
              str => str == Core.Block.empty_word ? x : str ++ x,
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
