type t = Block.t;

let init: t = [
  {
    pattern: ["bro"],
    expression: ["sum", "77", "5", "123"],
    value: ["205"],
  },
  {pattern: ["greeze"], expression: ["fact", "5"], value: ["120"]},
  {
    pattern: ["cloun"],
    expression: ["prod", "bro", "greeze"],
    value: ["24600"],
  },
  {
    pattern: ["foob"],
    expression: ["112", "+", "813", "+", "bro"],
    value: ["1135"],
  },
];
