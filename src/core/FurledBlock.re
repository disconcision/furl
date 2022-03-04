/*
  set acc=[]
  per row:
  1. go over expression, substituting from acc.
  2. if dead or zero uses, just drop it.
  3. if (alive or) multiple uses, leave it.
  4. if 1 use, drop and add to acc.
  (* with this strategy, shouldn't need to explictly transitively substitute)
  5. move on to next cell

  what we're left with should be convertible into let block
 */

[@deriving sexp]
type ctx = Environment.t_(Expression.form);

let rec substitute: (ctx, Expression.form) => Expression.form =
  (ctx, form) =>
    switch (form) {
    | Atom(a) =>
      switch (a) {
      | Lit(_)
      | Unbound(_)
      | Operator(_)
      | Formless(_) => form
      | Var(name, _) =>
        switch (Environment.lookup(ctx, name)) {
        | Some(thing) => thing
        | None => Atom(Var(name, []))
        }
      }
    | App(prim, fs) => App(prim, List.map(substitute(ctx), fs))
    | Seq(prim, fs) => Seq(prim, List.map(substitute(ctx), fs))
    | Let(_)
    | Unknown(_) => form
    };

let furl_block':
  (ctx, list(AnnotatedBlock.annotated_cell)) =>
  list(AnnotatedBlock.annotated_cell) =
  (ctx, ann_cells) => {
    let (new_cells, _) =
      List.fold_left(
        (
          (acc_cells, acc_ctx),
          {expression, pattern, vars, _} as cell: AnnotatedBlock.annotated_cell,
        ) => {
          let new_form = substitute(acc_ctx, expression.form);
          let new_cell = {
            ...cell,
            expression: {
              ...expression,
              form: new_form,
            },
          };
          let new_acc_ctx =
            switch (pattern.form) {
            | Some(Atom(Var(name, _))) =>
              Environment.extend(acc_ctx, (name, new_form))
            | _ => acc_ctx
            };
          switch (vars.binding_status, vars.num_refs) {
          | (Some(Dead), _) => (acc_cells, acc_ctx)
          | (_, Some(1)) => (acc_cells, new_acc_ctx)
          | (_, _n) => (acc_cells @ [new_cell], acc_ctx)
          };
        },
        ([], ctx),
        ann_cells,
      );
    new_cells;
  };

let cell_to_binding: AnnotatedBlock.annotated_cell => Expression.binding =
  ann_cell =>
    switch (ann_cell.pattern.form) {
    | Some(pat_form) => (pat_form, ann_cell.expression.form)
    | _ => (Unknown([]), ann_cell.expression.form)
    };

let furl_block: AnnotatedBlock.t => Expression.form =
  ({cells, _}) => {
    switch (cells |> furl_block'(Environment.empty) |> List.rev) {
    | [] => Unknown([])
    | [{expression: {form, _}, _}] => form
    | [{expression: {form, _}, _}, ...xs] =>
      Let(xs |> List.map(cell_to_binding) |> List.rev, form)
    };
  };
