open Js_of_ocaml;
open Incr_dom;
open Web;
open Sexplib.Std;

let on_display =
    (model: Model.t, old_model, state: State.t, ~schedule_action as _) =>
  model.animations_off || model === old_model
    ? () : Animate.animate_coords(state);

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

  let on_startup = (~schedule_action, _m: Model.t) => {
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
    schedule_action(Update.DoNothing);
    Os.is_mac :=
      Dom_html.window##.navigator##.platform##toUpperCase##indexOf(
        Js.string("MAC"),
      )
      >= 0;
    Async_kernel.Deferred.return(State.init_state);
  };

  let create = (model: Incr.t(Web.Model.t), ~old_model, ~inject) => {
    open Incr.Let_syntax;
    let%map model = model
    and old_model = old_model;
    //let {anim_targets, _}: Model.t = model;
    Component.create(
      ~apply_action=
        (update: Update.t, state: State.t, ~schedule_action) => {
          //Util.P.p(State.sexp_of_t(state));
          let m = Web.Update.apply(model, update, state, ~schedule_action);
          //Util.P.p(State.sexp_of_t(state));
          m;
        },
      /*
       ~on_display=
         (_, ~schedule_action as _) => {
          //Web.JsUtil.play_sound();
           print_endline("on_display");
         },
         */
      ~on_display=on_display(model, old_model),
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
