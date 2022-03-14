type props = {
  coords: Model.screen_coords,
  dims: (float, float),
};

let mk_props = (elem: Js_of_ocaml.Js.t(JsUtil.Dom_html.element)) => {
  let container_rect = elem##getBoundingClientRect;
  {
    coords: (
      int_of_float(container_rect##.top),
      int_of_float(container_rect##.left),
    ),
    dims: (
      Js_of_ocaml.Js.Optdef.get(container_rect##.height, _ => (-1.0)),
      Js_of_ocaml.Js.Optdef.get(container_rect##.width, _ => (-1.0)),
    ),
  };
};

let error_coords: Model.screen_coords = ((-1), (-1));

let get_props = (id): option(props) =>
  switch (JsUtil.get_elem_by_id_opt(id)) {
  | Some(elem) => Some(mk_props(elem))
  | None => None
  };

let get_coords = (id): Model.screen_coords =>
  switch (get_props(id)) {
  | Some({coords: (top, left), _}) => (top, left)
  | None => error_coords
  };

let delta_coords = ((new_x, new_y), (old_x, old_y)) =>
  (old_x, old_y) != error_coords ? (old_x - new_x, old_y - new_y) : (0, 0);

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

let set_style_init = ((id, (x, y))) =>
  JsUtil.set_style_by_id(id, init_transform(x, y, 1.0, 1.0));

let set_style_final = ((id, _)) =>
  JsUtil.set_style_by_id(id, final_tranform);

let set_init_coords = (target_ids: list(string), state: State.t) =>
  State.set_tracked_elems(
    state,
    List.map(i => (i, get_coords(i)), target_ids),
  );

let animate_coords = (state: State.t) => {
  let delta_coords =
    List.map(
      ((id, old)) => (id, delta_coords(get_coords(id), old)),
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
