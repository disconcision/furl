type t = Block.t;

let init': list((Name.s, Name.s)) = [
  (["bro"], ["sum", "77", "5", "123"]),
  (["greeze"], ["fact", "5"]),
  (["cloun"], ["prod", "bro", "greeze"]),
  (["foob"], ["112", "+", "813", "+", "bro"]),
];

let mk: list((Name.s, Name.s)) => t = List.map(Cell.init_full);

let init: t = init' |> mk |> Interpreter.run_block;
