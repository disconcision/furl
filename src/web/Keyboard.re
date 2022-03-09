open Virtual_dom.Vdom;

let keydown = (model: Model.t, key: string): list(Update.t) =>
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

let keyup = (_model: Model.t, key: string): list(Update.t) =>
  switch (key) {
  | "Shift" => [UpdateKeymap(km => {...km, shift: false})]
  | _ => []
  };

let seq = (~inj, updates) =>
  switch (updates) {
  | [] => Event.Ignore
  | [_, ..._] =>
    Event.(
      Many([Prevent_default, Stop_propagation, ...List.map(inj, updates)])
    )
  };

let handlers = (~inj: Update.t => Event.t, model: Model.t) => [
  Attr.on_keypress(_evt => Event.Prevent_default),
  Attr.on_keyup(evt => evt |> JsUtil.get_key |> keyup(model) |> seq(~inj)),
  Attr.on_keydown(evt => {
    let key = JsUtil.get_key(evt);
    print_endline("key pressed:");
    print_endline(key);
    let held = m => JsUtil.held(m, evt);
    let updates: list(Update.t) =
      if (!held(Ctrl) && !held(Alt) && !held(Meta)) {
        keydown(model, key);
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
    seq(~inj, updates);
  }),
];
