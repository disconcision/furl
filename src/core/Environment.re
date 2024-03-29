open Sexplib.Std;

[@deriving sexp]
type t_('a) = list((string, 'a));

let empty = [];

let is_empty =
  fun
  | [] => true
  | [_, ..._] => false;

let extend = (ctx, xa) => {
  let (x, _) = xa;
  [xa, ...List.remove_assoc(x, ctx)];
};

let lookup = (ctx, x) => List.assoc_opt(x, ctx);

let contains = (ctx, x) => List.mem_assoc(x, ctx);

let union = (ctx1, ctx2) => List.fold_left(extend, ctx2, ctx1);

let update = (ctx, key, f) => {
  // NOTE: this somewhat reorders the list
  switch (lookup(ctx, key)) {
  | Some(v) => [(key, f(v)), ...List.remove_assoc(key, ctx)]
  | None => ctx
  };
};

let update_or_extend = (ctx, key, f, v) => {
  // NOTE: this somewhat reorders the list
  switch (lookup(ctx, key)) {
  | Some(v') => [(key, f(v')), ...List.remove_assoc(key, ctx)]
  | None => [(key, v), ...List.remove_assoc(key, ctx)]
  };
};

let keys = ctx => List.map(((k, _)) => k, ctx);

let map = (f, xs) => List.map(((x, _) as xa) => (x, f(xa)), xs);

/*

 let filter = List.filter;

 let length = List.length;

 let to_list = ctx => ctx;
 */
