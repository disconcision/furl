let p = sexp => sexp |> Sexplib.Sexp.to_string_hum |> print_endline;

let p' = (str, sexp) =>
  sexp
  |> Sexplib.Sexp.to_string_hum
  |> (s => str ++ ": " ++ s)
  |> print_endline;

let ps = sss => {
  let _ = List.map(((a, b)) => p'(a, b), sss);
  ();
};
