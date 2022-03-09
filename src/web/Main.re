open Js_of_ocaml;
open Incr_dom;
open Web;

let init_coords = ((-1), (-1));

let init_ids = ["0", "1", "2", "3"];
let init_state = List.map(id => (id, init_coords), init_ids);
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

let delta_coords = (new_coords, old_coords) =>
  //state := new_coords;
  if (old_coords != new_coords && old_coords != init_coords) {
    let (old_x, old_y) = old_coords;
    let (new_x, new_y) = new_coords;
    (old_x - new_x, old_y - new_y);
  } else {
    (0, 0);
  };

let set_style1 = ((id, (delta_x, delta_y))) => {
  let style_str_1 =
    Printf.sprintf(
      "transform:translate(%dpx, %dpx); transition: transform 0s;",
      delta_y,
      delta_x,
    );
  let elem = force_get_elem_by_id(id);
  set_style(elem, style_str_1);
  ();
};

let set_style2 = ((id, _)) => {
  let style_str_2 = "transform:none; transition: transform 200ms; transition-timing-function:ease-out;";
  let elem = force_get_elem_by_id(id);
  set_style(elem, style_str_2);
  ();
};

let on_display = (model, old_model, state: State.t, ~schedule_action as _) =>
  if (model === old_model) {
    ();
  } else {
    let old_coords = state^;
    let new_coords: Core.Environment.t_((int, int)) =
      List.map(k => (k, get_coords(k)), init_ids);
    let delta_coords: Core.Environment.t_((int, int)) =
      List.map2(
        ((id_1, old_c), (_, new_c)) => (id_1, delta_coords(new_c, old_c)),
        old_coords,
        new_coords,
      );
    state := new_coords;
    request_frame(_ => {
      let _ = List.map(set_style1, delta_coords);
      request_frame(_ => {
        let _ = List.map(set_style2, delta_coords);
        ();
      });
    });
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

  let on_startup = (~schedule_action as _, _) => {
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

  let create = (model1: Incr.t(Web.Model.t), ~old_model, ~inject) => {
    open Incr.Let_syntax;
    let%map model = model1
    and old_model = old_model;
    Component.create(
      ~apply_action=Web.Update.apply(model),
      /*
       ~on_display=
         (_, ~schedule_action as _) => {
          //Web.JsUtil.play_sound();
           print_endline("on_display");
         },
         */
      ~on_display=on_display(model, old_model),
      model,
      Web.View.view(~inject, ~model),
    );
  };
};

Incr_dom.Start_app.start(
  (module App),
  ~debug=false,
  ~bind_to_element_with_id="container",
  ~initial_model=Web.Model.init,
);
