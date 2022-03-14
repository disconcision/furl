open Js_of_ocaml;
module Dom_html = Js_of_ocaml.Dom_html;

let get_elem_by_id_opt = id =>
  try(
    Some(
      Js.Opt.get(Dom_html.document##getElementById(Js.string(id)), () => {
        assert(false)
      }),
    )
  ) {
  | _ => None
  };

let set_elem_style = (elem, style: string) =>
  try(elem##setAttribute(Js.string("style"), Js.string(style))) {
  | _ => ()
  };

let set_style_by_id = (id: string, style: string) =>
  switch (get_elem_by_id_opt(id)) {
  | Some(elem) => set_elem_style(elem, style)
  | None => ()
  };

let request_frame = kont => {
  let _ = Dom_html.window##requestAnimationFrame(Js.wrap_callback(kont));
  ();
};

type mod_key =
  | Shift
  | Alt
  | Ctrl
  | Meta;

let held = (m: mod_key, evt) =>
  switch (m) {
  | Shift => Js.to_bool(evt##.shiftKey)
  | Alt => Js.to_bool(evt##.altKey)
  | Ctrl => Js.to_bool(evt##.ctrlKey)
  | Meta => Js.to_bool(evt##.metaKey)
  };

let get_key = evt =>
  Js.to_string(Js.Optdef.get(evt##.key, () => failwith("JsUtil.get_key")));

let date_now = () => {
  %js
  new Js.date_now;
};

/*
 open WebAudio;
 let play_sound = () => {
   let context = {
     %js
     new WebAudio.audioContext;
   };
   //const audioElement = document.querySelector('audio');
   //const track = audioContext.createMediaElementSource(audioElement);
   /*
    let audioElement = Dom_html.document##querySelector(Js.string("audio"));
    let track =
      context##createMediaElementSource(
        (audioElement :> Js.t(Dom_html.mediaElement)),
      );*/
   let oscillator = context##createOscillator;
   oscillator##.frequency##.value := 200.0;
   oscillator##._type := Js.string("sine");
   let blah = context##.destination;
   oscillator##connect((blah :> Js.t(WebAudio.audioNode)));
   //oscillator##start;
   let _ =
     Js.Unsafe.coerce(Dom_html.document)##audioPlay(
       Js.string("test-meeee.m4a"),
     );
   print_endline("oscillator_started");
 };
 */

/*
 let context = new%js WebAudio.audioContext in

     let oscillator = context##createOscillator in
     oscillator##.frequency##.value := 200.0;
     oscillator##._type := (Js.string "sine");
     oscillator##(connect ((context##.destination :> WebAudio.audioNode Js.t)));

     debug "about to start oscillator";
     oscillator##start;
     debug "oscillator started";
 */
