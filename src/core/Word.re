open Sexplib.Std;

[@deriving sexp]
type t = string;
[@deriving sexp]
type s = list(t);

let empty: t = "🕳️";

let var_regex =
  Re.Str.regexp("^\\([a-zA-Z]\\|_[_a-zA-Z0-9]\\)[_a-zA-Z0-9']*$");
let is_valid_var = s => Re.Str.string_match(var_regex, s, 0);

let running_index = ref(3);

let running_names = [
  "blarg",
  "freezepop",
  "crork",
  "foo",
  "cruby",
  "baz",
  "crunk",
  "dree",
  "bar",
  "gruben",
  "bro",
  "weeb",
  "grug",
  "carrudy",
];
let emoji_names = [
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

// idea: when emoji-names are selected,
// make uses/bindings animated, slowly growing and shrinking

//TODO: better approach
let emoji_of_default: string => string =
  fun
  | "blarg" => "📎"
  | "freezepop" => "🌽"
  | "crork" => "💭"
  | "foo" => "🌘"
  | "cruby" => "🍸"
  | "baz" => "🎈"
  | "crunk" => "🍋"
  | "dree" => "🐠"
  | "bar" => "🖍"
  | "gruben" => "🤘"
  | "bro" => "🍮"
  | "weeb" => "👌"
  | "grug" => "🌈"
  | "carrudy" => "🐿"
  | "add" => "add"
  | "mult" => "mult"
  | "fact" => "fact"
  | _ => "🤔";

let get_name = (): string => {
  let i = running_index^;
  running_index := running_index^ + 1;
  List.nth(running_names, i mod List.length(running_names));
};
