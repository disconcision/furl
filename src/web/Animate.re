let error_coords: Model.screen_coords = ((-1), (-1));

let get_coords = (id): Model.screen_coords =>
  switch (JsUtil.get_elem_by_id_opt(id)) {
  | Some(elem) =>
    let container_rect = elem##getBoundingClientRect;
    (
      int_of_float(container_rect##.top),
      int_of_float(container_rect##.left),
    );
  | None => error_coords
  };

let delta_coords = ((new_x, new_y), (old_x, old_y)) =>
  (old_x, old_y) != error_coords ? (old_x - new_x, old_y - new_y) : (0, 0);

let pos_timing_fn = "cubic-bezier(0.75, -0.5, 0.25, 1.5)";
let pos_duration = "150ms";

let init_transform = (x, y) =>
  Printf.sprintf(
    "transform:translate(%dpx, %dpx); transition: transform 0s;",
    y,
    x,
  );

let final_tranform =
  Printf.sprintf(
    "transform:none; transition: transform %s %s;",
    pos_duration,
    pos_timing_fn,
  );

let set_style_init = ((id, (x, y))) =>
  JsUtil.set_style_by_id(id, init_transform(x, y));

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