open Virtual_dom.Vdom;
open Core;

let handlers = (~inject: FurlUpdate.t => Event.t, _model) => [
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
    let updates: list(FurlUpdate.t) =
      if (!held(Ctrl) && !held(Alt) && !held(Meta)) {
        switch (key) {
        | "ArrowRight" => [] //[MoveRight]
        | "ArrowLeft"
        | "ArrowUp"
        | "ArrowDown"
        | "Backspace"
        | "Delete"
        | _ => []
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
