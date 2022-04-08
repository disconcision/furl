open Sexplib.Std;

[@deriving sexp]
type t = int;
let cell_uid_gen: ref(t) = ref(0);
let ids: ref(list(t)) = ref([]);

let mk = (): t => {
  let uid = cell_uid_gen^;
  cell_uid_gen := cell_uid_gen^ + 1;
  ids := [uid, ...ids^];
  uid;
};
