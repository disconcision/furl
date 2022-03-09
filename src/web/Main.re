open Js_of_ocaml;
open Incr_dom;
open Web;

open Sexplib.Std;
[@deriving sexp]
type bloo = list((string, (int, int)));
let init_coords = ((-1), (-1));

let init_state = List.map(id => (id, init_coords), Update.cell_targets_todo);
let log = x => Js_of_ocaml.Firebug.console##log(x);

let force_get_elem_by_id = id => {
  let doc = Dom_html.document;
  Js.Opt.get(
    doc##getElementById(Js.string(id)),
    () => {
      log(id);
      assert(false);
    },
  );
};
let get_coords = (id): 'a =>
  try({
    let container_rect = force_get_elem_by_id(id)##getBoundingClientRect;
    (
      int_of_float(container_rect##.top),
      int_of_float(container_rect##.left),
    );
  }) {
  | _ => init_coords
  };

let request_frame = c => {
  let _ = Dom_html.window##requestAnimationFrame(Js.wrap_callback(c));
  ();
};

let set_style = (elem, string) =>
  elem##setAttribute(Js.string("style"), Js.string(string));

let set_style_or_dont = (elem_id, string) =>
  try(set_style(force_get_elem_by_id(elem_id), string)) {
  | _ => ()
  };

let delta_coords = ((new_x, new_y), (old_x, old_y)) =>
  (old_x, old_y) != init_coords ? (old_x - new_x, old_y - new_y) : (0, 0);

let set_style_init = ((id, (x, y))) =>
  Printf.sprintf(
    "transform:translate(%dpx, %dpx); transition: transform 0s;",
    y,
    x,
  )
  |> set_style_or_dont(id);

let set_style_final = ((id, _)) =>
  set_style_or_dont(
    id,
    "transform:none; transition: transform 200ms ease-out;",
  );

let flip = (old_coords, new_coords, anim_targets) => {
  let delta_coords: Core.Environment.t_((int, int)) =
    List.map2(
      ((id, old), (_, nw)) => (id, delta_coords(nw, old)),
      old_coords,
      new_coords,
    )
    |> List.filter(((id, _)) => List.mem(id, anim_targets));
  //Util.P.p(sexp_of_bloo(delta_coords));
  List.iter(set_style_init, delta_coords);
  request_frame(_ => List.iter(set_style_final, delta_coords));
};

let on_display =
    (
      model: Model.t,
      old_model,
      anim_targets,
      state: State.t,
      ~schedule_action as _,
    ) =>
  if (model.animations_off || model === old_model) {
    ();
  } else {
    let old_coords = state^;
    let new_coords = List.map(((i, _)) => (i, get_coords(i)), old_coords);
    //Util.P.p(sexp_of_bloo(old_coords));
    //Util.P.p(sexp_of_bloo(new_coords));
    state := new_coords;
    flip(old_coords, new_coords, anim_targets);
  };

module App = {
  module Model = Model;
  module Action = Update;
  module State = State;

  /*
   let observe_font_specimen = (id, update) =>
     ResizeObserver.observe(
       ~node=JsUtil.get_elem_by_id(id),
       ~f=
         (entries, _) => {
           let specimen = Js.to_array(entries)[0];
           let rect = specimen##.contentRect;
           update(
             Web.FontMetrics.{
               row_height: rect##.bottom -. rect##.top,
               col_width: rect##.right -. rect##.left,
             },
           );
         },
       (),
     );
     */

  let on_startup = (~schedule_action as _, _m: Model.t) => {
    /*
     let _ =
       observe_font_specimen("font-specimen", fm =>
         schedule_action(Web.Update.SetFontMetrics(fm))
       );
     let _ =
       observe_font_specimen("logo-font-specimen", fm =>
         schedule_action(Web.Update.SetLogoFontMetrics(fm))
       );
       */
    Os.is_mac :=
      Dom_html.window##.navigator##.platform##toUpperCase##indexOf(
        Js.string("MAC"),
      )
      >= 0;
    Async_kernel.Deferred.return(ref(init_state));
  };

  let create = (model: Incr.t(Web.Model.t), ~old_model, ~inject) => {
    open Incr.Let_syntax;
    let%map model = model
    and old_model = old_model;
    let {anim_targets, _}: Model.t = model;
    Component.create(
      ~apply_action=
        (update: Update.t, state: State.t, ~schedule_action) => {
          let model = Update.update_anim_targets(_ => [], model);
          Web.Update.apply(model, update, state, ~schedule_action);
        },
      /*
       ~on_display=
         (_, ~schedule_action as _) => {
          //Web.JsUtil.play_sound();
           print_endline("on_display");
         },
         */
      ~on_display=on_display(model, old_model, anim_targets),
      model,
      Web.View.view(~inj=inject, ~model),
    );
  };
};

Incr_dom.Start_app.start(
  (module App),
  ~debug=false,
  ~bind_to_element_with_id="container",
  ~initial_model=Web.Model.init,
);
