open Virtual_dom.Vdom;
open Update;

let keydown = (key: string, model: Model.t) =>
  switch (key) {
  | "F3" => [DebugPrint]
  | "Shift" => [UpdateKeymap(km => {...km, shift: true})]
  | "ArrowDown" when model.keymap.shift => [UniFocus(SwapCellDown)]
  | "ArrowUp" when model.keymap.shift => [UniFocus(SwapCellUp)]
  | "ArrowRight" => [UniFocus(MoveRight)]
  | "ArrowLeft" => [UniFocus(MoveLeft)]
  | "ArrowUp" => [UniFocus(MoveUp)]
  | "ArrowDown" => [UniFocus(MoveDown)]
  | "Enter" when model.world == [] => [InsertNewCell(0)]
  | " " when model.world == [] => [InsertNewCell(0)]
  | "Enter" => [UniFocus(InsertNewCellF)]
  | " " => [UniFocus(InsertNewWordF)]
  | "Delete" => [UniFocus(DeleteF)]
  | "Backspace" => [UniFocus(Backspace)]
  | x => [UniFocus(InsertChar(x))]
  };

let keyup = (key: string, _model: Model.t) =>
  switch (key) {
  | "Shift" => [UpdateKeymap(km => {...km, shift: false})]
  | _ => []
  };

let seq = (~inject, updates) =>
  switch (updates) {
  | [] => Event.Ignore
  | [_, ..._] =>
    Event.(
      Many([Prevent_default, Stop_propagation, ...List.map(inject, updates)])
    )
  };

let handlers = (~inject: Update.t => Event.t, model: Model.t) => [
  Attr.on_keypress(_evt => Event.Prevent_default),
  Attr.on_keyup(evt => {
    let key = JsUtil.get_key(evt);
    let updates: list(Update.t) = {
      print_endline("key pressed:");
      print_endline(key);
      keyup(key, model);
    };
    seq(~inject, updates);
  }),
  Attr.on_keydown(evt => {
    let key = JsUtil.get_key(evt);
    let held = m => JsUtil.held(m, evt);
    let updates: list(Update.t) =
      if (!held(Ctrl) && !held(Alt) && !held(Meta)) {
        print_endline("key pressed:");
        print_endline(key);
        keydown(key, model);
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
    seq(~inject, updates);
  }),
];
