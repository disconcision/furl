let lcell = (idx, b: Block.t) => List.nth(b, idx);
let lexp = (f: Cell.t) => f.expression;
let lpat = (f: Cell.t) => f.pattern;
let lval = (f: Cell.t) => f.value;
let lword = (idx, b) => List.nth(b, idx);

let first_exp_len = w => w |> lcell(0) |> lexp |> List.length;
let first_word_len = w => w |> lcell(0) |> lexp |> lword(0) |> String.length;
