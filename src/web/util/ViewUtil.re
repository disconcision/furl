open Virtual_dom.Vdom;
open Virtual_dom.Vdom.Node;

module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;

let divc = (cls, contents) => div([Attr.class_(cls)], contents);

let stop = a => Event.Many([Event.Stop_propagation, a]);

let prevent = a => Event.Many([Event.Prevent_default, a]);

let hash_of_string = str =>
  List.fold_left(
    (acc, c) => acc + int_of_char(c),
    0,
    List.of_seq(String.to_seq(str)),
  );

let random_offset = (~bound_x=5, ~bound_y=8, seed_str) => {
  Random.init(hash_of_string(seed_str));
  let (x, y) = (
    Random.int(bound_x) - bound_x / 2,
    Random.int(bound_y) - bound_y / 2,
  );
  Attr.string_property(
    "style",
    "position: relative; left: "
    ++ string_of_int(x)
    ++ "px; top: "
    ++ string_of_int(y)
    ++ "px;",
  );
};

let random_skew = (~bound_x=32, ~bound_y=1.2, seed_str) => {
  Random.init(hash_of_string(seed_str));
  let (x, y) = (
    Random.int(bound_x) - bound_x / 2,
    Random.float(bound_y) -. bound_y /. 2.,
  );
  Attr.string_property(
    "style",
    Printf.sprintf("transform: SkewY(%fdeg) SkewX(%ddeg);", y, x),
  );
};

/*
 Attr.on("dragover", evt => {
          let container_rect =
            JsUtil.get_elem_by_id("root")##getBoundingClientRect;
          let (target_x, target_y) = (
            float_of_int(evt##.clientX),
            float_of_int(evt##.clientY),
          );
          let blee =
            Float.to_int(Float.round(target_y -. container_rect##.top) /. 30.);
          let blah =
            Float.to_int(
              Float.round(target_x -. container_rect##.left) /. 30.,
            );
          print_endline(string_of_int(evt##.clientX));
          print_endline(string_of_int(evt##.clientY));
          print_endline(string_of_int(blee));
          print_endline(string_of_int(blah));
          Event.Prevent_default;
        })
 */
