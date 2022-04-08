open Sexplib.Std;

[@deriving sexp]
type t = {
  uid: UID.t,
  name: Name.t,
};

[@deriving sexp]
type s = list(t);

let ids: ref(list(UID.t)) = ref([]);

let is_empty: t => bool = ({name, _}) => name == Name.empty;

let mk_name = (name: Name.t) => {
  let uid = UID.mk();
  ids := [uid, ...ids^];
  {uid, name};
};

let mk_empty = () => mk_name(Name.empty);

let mk = () => mk_name(Name.mk());

let mapn = (f: Name.t => Name.t, {name, _} as w: t) => {
  ...w,
  name: f(name),
};
