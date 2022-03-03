open Sexplib.Std;

[@deriving sexp]
type t = string;
[@deriving sexp]
type s = list(t);

let empty: t = "🕳️";

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
