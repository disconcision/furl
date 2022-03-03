open Sexplib.Std;

[@deriving sexp]
type t = string;
[@deriving sexp]
type s = list(t);

let empty: t = "🕳️";

let var_regex =
  Re.Str.regexp("^\\([a-zA-Z]\\|_[_a-zA-Z0-9]\\)[_a-zA-Z0-9']*$");
let is_valid_var = s => Re.Str.string_match(var_regex, s, 0);

let running_index = ref(0);

let _running_names = [
  "foo",
  "cruby",
  "baz",
  "crunk",
  "dree",
  "bar",
  "gruben",
  "bro",
  "freezepop",
  "weeb",
  "grug",
  "carrudy",
];
let running_names = [
  "📎",
  "🌽",
  "💭",
  "🌘",
  "🍸",
  "🎈",
  "🍋",
  "🐠",
  "🖍",
  "🤘",
  "🍮",
  "👌",
  "🌈",
  "🐿",
];

let get_name = (): string => {
  let i = running_index^;
  running_index := running_index^ + 1;
  List.nth(running_names, i mod List.length(running_names));
};
