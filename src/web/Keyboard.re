open Virtual_dom.Vdom;
open Update;

let handlers = (~inject: Update.t => Event.t, model: Model.t) => [
  Attr.on_keypress(_evt => Event.Prevent_default),
  Attr.on_keyup(evt => {
    let key = JsUtil.get_key(evt);
    let updates: list(Update.t) = {
      print_endline("key pressed:");
      print_endline(key);
      switch (key) {
      | "Shift" => [UpdateKeymap(km => {...km, shift: false})]
      | _ => []
      };
    };
    switch (updates) {
    | [] => Event.Ignore
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
  Attr.on_keydown(evt => {
    let key = JsUtil.get_key(evt);
    let held = m => JsUtil.held(m, evt);
    let updates: list(Update.t) =
      if (!held(Ctrl) && !held(Alt) && !held(Meta)) {
        print_endline("key pressed:");
        print_endline(key);
        switch (key) {
        | "F3" => [DebugPrint]
        | "Shift" => [UpdateKeymap(km => {...km, shift: true})]
        | "ArrowDown" when model.keymap.shift => [UniFocus(SwapCellDown)]
        | "ArrowUp" when model.keymap.shift => [UniFocus(SwapCellUp)]
        | "ArrowRight" => [UniFocus(MoveRight)]
        | "ArrowLeft" => [UniFocus(MoveLeft)]
        | "ArrowUp" => [UniFocus(MoveUp)]
        | "ArrowDown" => [UniFocus(MoveDown)]
        | "Enter" =>
          switch (model.world) {
          | [] => [InsertNewCell(0)]
          | _ => [UniFocus(InsertNewCellF)]
          }
        | " " =>
          switch (model.world) {
          | [] => [InsertNewCell(0)]
          | _ => [UniFocus(InsertNewWordF)]
          }
        | "Delete" => [UniFocus(DeleteF)]
        | "Backspace" => [UniFocus(Backspace)]
        | x => [UniFocus(InsertChar(x))]
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
