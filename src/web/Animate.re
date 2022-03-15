let mk_props = (elem: Js_of_ocaml.Js.t(JsUtil.Dom_html.element)): Model.box => {
  let container_rect = elem##getBoundingClientRect;
  {
    top: int_of_float(container_rect##.top),
    left: int_of_float(container_rect##.left),
    height: Js_of_ocaml.Js.Optdef.get(container_rect##.height, _ => (-1.0)),
    width: Js_of_ocaml.Js.Optdef.get(container_rect##.width, _ => (-1.0)),
  };
};

let get_box = (id): option(Model.box) =>
  switch (JsUtil.get_elem_by_id_opt(id)) {
  | Some(elem) => Some(mk_props(elem))
  | None => None
  };

let delta_box = (init: Model.box, final: Model.box): Model.box => {
  left: final.left - init.left,
  top: final.top - init.top,
  width: final.width -. init.width,
  height: final.height -. init.height,
};

let delta_box_opt =
    (init: option(Model.box), final: option(Model.box)): option(Model.box) =>
  switch (final, init) {
  | (Some(final), Some(init)) => Some(delta_box(init, final))
  | _ => None
  };

let pos_timing_fn = "cubic-bezier(0.75, -0.5, 0.25, 1.5)";
let pos_duration = "150ms";

let init_transform = (x, y, sx, sy) =>
  Printf.sprintf(
    "transform:translate(%dpx, %dpx) scale(%f, %f); transition: transform 0s;",
    y,
    x,
    sy,
    sx,
  );

let final_tranform =
  Printf.sprintf(
    "transform:none; transition: transform %s %s;",
    pos_duration,
    pos_timing_fn,
  );

let set_style_init = ((id, box: option(Model.box))) =>
  switch (box) {
  | None => ()
  | Some(box) =>
    JsUtil.set_style_by_id(id, init_transform(box.top, box.left, 1.0, 1.0))
  };

let set_style_final = ((id, _)) =>
  JsUtil.set_style_by_id(id, final_tranform);

let set_init_coords = (target_ids: list(string), state: State.t) =>
  State.set_tracked_elems(
    state,
    List.map(i => (i, get_box(i)), target_ids),
  );

let animate_coords = (state: State.t) => {
  let delta_coords =
    List.map(
      ((id, box)) => (id, delta_box_opt(get_box(id), box)),
      State.get_tracked_elems(state),
    );
  List.iter(set_style_init, delta_coords);
  JsUtil.request_frame(_ => List.iter(set_style_final, delta_coords));
  State.set_tracked_elems(state, []);
};

let cells = (state: State.t) =>
  set_init_coords(Core.Cell.ids^ |> List.map(string_of_int), state);

let cells_except = (id: Core.Cell.uid, state: State.t) =>
  set_init_coords(
    Core.Cell.ids^ |> List.filter((!=)(id)) |> List.map(string_of_int),
    state,
  );
