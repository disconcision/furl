open Sexplib.Std;
open Util;
open Core;

[@deriving sexp]
type single_focus_action =
  | SwapCellDown
  | SwapCellUp
  | MoveDown
  | MoveUp
  | MoveRight
  | MoveLeft
  | InsertChar(string)
  | UpdateWordF(Name.t => Name.t)
  | DeleteF
  | InsertNewWordF
  | InsertNewCellF
  | Backspace;

[@deriving sexp]
type t =
  | DoNothing
  | SetLastKey(Model.lastkey)
  //| SetAnimTargetCells
  | SetFocus(Model.focus)
  | UniFocus(single_focus_action)
  | Delete(Path.t)
  | TogglePatternDisplay
  | ToggleAnimations
  | UpdateKeymap(Model.keymap => Model.keymap)
  | DebugPrint
  // words
  | UpdateWord(Path.t, Name.t => Name.t)
  | InsertWord(Path.t, Model.word_sep_id, Word.t)
  | InsertNewWord(Path.t, Model.word_sep_id)
  // cells
  | InsertCell(Block.cell_id, Cell.t)
  | InsertNewCell(Block.cell_id)
  | ReorderCell(Block.cell_id, Model.cell_sep_id)
  | SwapCells(Block.cell_id, Block.cell_id)
  // drag-n-drop
  | Pickup(Model.carry)
  | SetDropTarget(Model.drop_target)
  | DropOnWord(Path.t)
  | DropOnWordSep(Path.t, Model.word_sep_id)
  | DropOnCellSep(Model.cell_sep_id)
  | DeleteCarrySource
  // trash
  | TrashCarry(Model.screen_coords)
  | PickupTrash(Model.trash_idx)
  | EmptyTrash;

let update_focus = (f, {focus, _} as model: Model.t) => {
  ...model,
  focus: f(focus),
};

let update_world = (f, {world, _} as model: Model.t) => {
  ...model,
  world: f(world),
};

let update_carry = (f, {carry, _} as model: Model.t) => {
  ...model,
  carry: f(carry),
};

let update_drop_target = (f, {drop_target, _} as model: Model.t) => {
  ...model,
  drop_target: f(drop_target),
};

let update_trash = (f, {trash, _} as model: Model.t) => {
  ...model,
  trash: f(trash),
};

let update_pattern_display = (f, {pattern_display, _} as model: Model.t) => {
  ...model,
  pattern_display: f(pattern_display),
};

let update_keymap = (f, {keymap, _} as model: Model.t) => {
  ...model,
  keymap: f(keymap),
};

let update_animations_off = (f, {animations_off, _} as model: Model.t) => {
  ...model,
  animations_off: f(animations_off),
};

let rec apply: (Model.t, t, 'b, ~schedule_action: 'a) => Model.t =
  (model: Model.t, update: t, state: State.t, ~schedule_action) => {
    //let model = update_drop_target(_ => NoTarget, model);
    let app = (a, m) => apply(m, a, state, ~schedule_action);
    let model =
      switch (update) {
      | DoNothing => model
      | SetLastKey(k) =>
        switch (model.lastkey, k) {
        | (KeyDown(qq), KeyDown(bb)) when qq == bb => ()
        | _ => ()
        };
        {...model, lastkey: k};
      | SetFocus(focus) => update_focus(_ => focus, model)
      | Pickup(thing) => update_carry(_ => thing, model)
      | EmptyTrash => update_trash(_ => [], model)
      | UpdateKeymap(f) => update_keymap(f, model)
      | SetDropTarget(target) => update_drop_target(_ => target, model)
      | SwapCells(a, b) => update_world(ListUtil.swap(a, b), model)
      | Delete(path) => update_world(Path.delete(path), model)
      | UpdateWord(path, f) =>
        update_world(Path.update_word(f, path), model)
      | UniFocus(a) => apply_single(a, model, state, ~schedule_action)
      | PickupTrash(idx) =>
        //TODO: retain trash if not restored
        switch (List.nth(model.trash, idx)) {
        | TrashedWord(word, _) =>
          model
          |> update_trash(ListUtil.remove(idx))
          |> app(Pickup(WordBrush(word.name)))
        | TrashedCell(cell, _) =>
          model
          |> update_trash(ListUtil.remove(idx))
          |> app(Pickup(CellBrush(cell)))
        }
      | DeleteCarrySource =>
        switch (model.carry) {
        | NoCarry
        | WordBrush(_)
        | CellBrush(_) => model
        | WordExp({path, _})
        | WordPat({path, _})
        | Cell({path, _}) => app(Delete(path), model)
        }
      | TrashCarry(coords) =>
        switch (model.carry) {
        | WordExp({path, _}) =>
          let word =
            switch (Path.get_word(path, model.world)) {
            | Some(word) => word
            | None => Word.mk_name("lol. lmao.")
            };
          model
          |> update_trash(trash => [TrashedWord(word, coords), ...trash])
          |> app(DeleteCarrySource);
        | Cell({path, _}) =>
          // TODO: refactor to use annotated_cell
          let cell =
            switch (Path.get_cell(path, model.world)) {
            | Some(cell) => cell
            | None => Cell.init()
            };
          Animate.cells(state);
          model
          |> update_trash(trash => [TrashedCell(cell, coords), ...trash])
          |> app(DeleteCarrySource);
        | _ => model
        }
      | InsertCell(sep_idx, cell) =>
        //TODO: index 666, cleanup
        model
        |> update_world(Path.insert_cell(sep_idx, cell))
        |> app(
             SetFocus(
               SingleCell([
                 Cell(Index(sep_idx, 1 + List.length(model.world))),
                 Field(Expression),
                 Word(Index(0, 666)),
               ]),
             ),
           )
      | InsertNewCell(sep_idx) =>
        let cell = Core.Cell.init();
        Animate.cells_and(cell.uid, state);
        app(InsertCell(sep_idx, cell), model);
      | ReorderCell(cell_idx, new_idx) =>
        let cell = Block.nth_cell(model.world, cell_idx);
        let new_idx = new_idx > cell_idx ? new_idx - 1 : new_idx;
        model
        |> app(Delete([Cell(Index(cell_idx, List.length(model.world)))]))
        |> app(InsertCell(new_idx, cell));
      | DropOnCellSep(sep_idx) =>
        let block = model.world;
        (
          switch (model.carry) {
          // 1. unbound names get new cells
          | WordExp({form: Unbound(name), path, _})
              when Path.is_cell_idx((==)(sep_idx), path) =>
            let cell = Cell.init_name(name);
            app(InsertCell(sep_idx, cell), model);
          // 2. literals get abstracted
          | WordExp({form: Lit(lit), path, _})
              when Path.is_cell_idx((==)(sep_idx), path) =>
            switch (Path.cell_idx(path)) {
            | Some(i) when sep_idx == i =>
              let incr_path = Path.update_cell_idx((+)(1), path);
              let (word, cell) = Cell.init_w(Expression.string_of_lit(lit));
              model
              |> app(InsertCell(sep_idx, cell))
              |> app(UpdateWord(incr_path, _ => word.name));
            | _ => model
            }
          // 3. cells get copied if shift is pressed
          | Cell({path: [Cell(Index(carry_idx, _)), ..._], _})
              when model.keymap.shift =>
            let cell = carry_idx |> Block.nth_cell(block) |> Cell.copy;
            Animate.cells_except(cell.uid, state);
            app(InsertCell(sep_idx, cell), model);
          // 4. cells get reordered otherwise
          | Cell({path: [Cell(Index(carry_idx, _)), ..._], uid, _}) =>
            Animate.cells_except(uid, state);
            let cell = Block.nth_cell(block, carry_idx);
            let new_idx = sep_idx > carry_idx ? sep_idx - 1 : sep_idx;
            model
            |> app(Delete([Cell(Index(carry_idx, List.length(block)))]))
            |> app(InsertCell(new_idx, cell));
          // 5. restore cell from trash
          | CellBrush(cell) =>
            Animate.cells(state);
            app(InsertCell(sep_idx, cell), model);
          | _ => model
          }
        )
        |> app(Pickup(NoCarry));
      | InsertWord(path, sep_idx, new_word) =>
        let m =
          update_world(Path.insert_word(new_word, path, sep_idx), model);
        app(
          switch (path) {
          | [c, f, ..._] =>
            //TODO: cleanup
            let k =
              switch (Path.get_words([c, f], m.world)) {
              | Some(ws) => List.length(ws)
              | None => 666
              };
            SetFocus(SingleCell([c, f, Word(Index(sep_idx, k))]));
          | _ => SetFocus(SingleCell(path))
          },
          m,
        );
      | InsertNewWord(path, sep_idx) =>
        app(InsertWord(path, sep_idx, Core.Word.mk_empty()), model)
      | DropOnWordSep(path, sep_idx) =>
        (
          switch (model.carry) {
          | _ when Path.is_word_sep_touching_empty(path, sep_idx, model.world) =>
            // HACK? annoying to allow drop if next to empty word
            model
          | WordPat({path: origin_path, word, _}) =>
            switch (Path.cell_idx(path)) {
            | Some(target_cell_idx)
                when Path.is_cell_idx((>)(target_cell_idx), origin_path) =>
              model |> app(InsertWord(path, sep_idx, word))
            | _ => model
            }
          | WordExp({word, _}) when model.keymap.shift =>
            // if holding shift, copy instead of move
            app(InsertWord(path, sep_idx, word), model)
          | WordExp({
              path: [_, _, Word(Index(word_idx, _)), ..._] as word_path,
              word,
              _,
            })
              when !model.keymap.shift =>
            if (sep_idx > word_idx) {
              model
              |> app(InsertWord(path, sep_idx, word))
              |> app(Delete(word_path));
            } else {
              model
              |> app(Delete(word_path))
              |> app(InsertWord(path, sep_idx, word));
            }
          | WordBrush(name) =>
            app(InsertWord(path, sep_idx, Word.mk_name(name)), model)
          | _ => model
          }
        )
        // hack? sometimes ondragleave doesn't get triggered when dropping
        //|> update_drop_target(_ => NoTarget)
        |> app(Pickup(NoCarry))
      | DropOnWord(path) =>
        // TODO: figure out how to combine this with droponwordsep
        switch (path) {
        | [_, _, Word(Index(path_word_idx, _)), ..._] =>
          (
            switch (model.carry) {
            | WordPat({path: origin_path, word, _}) =>
              switch (Path.cell_idx(path)) {
              | Some(target_cell_idx)
                  when Path.is_cell_idx((>)(target_cell_idx), origin_path) =>
                model
                |> app(Delete(path))
                |> app(InsertWord(path, path_word_idx, word))
              | _ => model
              }
            | WordExp({word, _}) when model.keymap.shift =>
              // if holding shift, copy instead of move
              model
              |> app(Delete(path))
              |> app(InsertWord(path, path_word_idx, word))
            | WordExp({
                path: [_, _, Word(Index(word_idx, _)), ..._] as word_path,
                word,
                _,
              })
                when !model.keymap.shift =>
              if (path_word_idx == word_idx) {
                model;
              } else if (path_word_idx > word_idx) {
                model
                |> app(InsertWord(path, path_word_idx, word))
                |> app(Delete(word_path))
                |> app(Delete(path));
              } else {
                model
                |> app(Delete(word_path))
                |> app(Delete(path))
                |> app(InsertWord(path, path_word_idx, word));
              }
            | WordBrush(name) =>
              model
              |> app(Delete(path))
              |> app(InsertWord(path, path_word_idx, Word.mk_name(name)))
            | _ => model
            }
          )
          |> app(Pickup(NoCarry))
        | _ => model
        }
      | TogglePatternDisplay =>
        update_pattern_display(
          x =>
            switch (x) {
            | Name => Emoji
            | Emoji => Name
            },
          model,
        )
      | ToggleAnimations => update_animations_off(b => !b, model)
      | DebugPrint =>
        let ann_block = AnnotatedBlock.mk(model.world);
        let furled = FurledBlock.furl_block(ann_block);
        Util.P.ps([
          ("FOCUS:", Model.sexp_of_focus(model.focus)),
          ("BLOCK:", Block.sexp_of_t(model.world)),
          ("ANN:", AnnotatedBlock.sexp_of_t(ann_block)),
          ("FURLED:", Expression.sexp_of_t(furled)),
        ]);
        model;
      };
    update_world(Interpreter.run_block, model);
  }

and apply_single:
  (single_focus_action, Model.t, 'b, ~schedule_action: 'a) => Model.t =
  (a, model, state, ~schedule_action) => {
    let SingleCell(current_path) = model.focus;
    let app = (a, m) => apply(m, a, state, ~schedule_action);
    let update_focus = (f: (Block.t, Path.t) => Path.t, m: Model.t) => {
      app(SetFocus(SingleCell(f(m.world, current_path))), m);
    };
    switch (a) {
    | UpdateWordF(f) => app(UpdateWord(current_path, f), model)
    | DeleteF =>
      // TODO: set focus
      switch (model.focus) {
      | SingleCell(path) => app(Delete(path), model)
      }
    | InsertNewCellF =>
      let SingleCell(current_path) = model.focus;
      switch (current_path) {
      | [Cell(Index(cell_idx, _)), ..._] =>
        app(InsertNewCell(cell_idx + 1), model)
      | _ => model
      };
    | InsertNewWordF =>
      let SingleCell(current_path) = model.focus;
      switch (current_path) {
      | [_, _, Word(Index(n, _)), ..._]
          when
            switch (Path.get_word(current_path, model.world)) {
            | Some(word) when !Word.is_empty(word) => true
            | _ => false
            } =>
        app(InsertNewWord(current_path, n + 1), model)
      | _ => model
      };
    | SwapCellDown =>
      switch (current_path) {
      | [Cell(Index(cell_idx, k))] when cell_idx != k - 1 =>
        Animate.cells(state);
        model
        |> app(ReorderCell(cell_idx + 1, cell_idx))
        |> app(SetFocus(SingleCell([Cell(Index(cell_idx + 1, k))])));
      | _ => model
      }
    | SwapCellUp =>
      switch (current_path) {
      | [Cell(Index(cell_idx, k))] when cell_idx != 0 =>
        Animate.cells(state);
        model
        |> app(ReorderCell(cell_idx, cell_idx - 1))
        |> app(SetFocus(SingleCell([Cell(Index(cell_idx - 1, k))])));
      | _ => model
      }
    | MoveDown => update_focus(Path.down_path, model) //|> app(SetAnimTargets(["-1"]))
    | MoveUp => update_focus(Path.up_path, model) //|> app(SetAnimTargets(["-1"]))
    | MoveRight => update_focus(Path.next_word_path, model)
    //|> app(SetAnimTargets(["-1"]))
    | MoveLeft => update_focus(Path.prev_word_path, model)
    //|> app(SetAnimTargets(["-1"]))
    | InsertChar(op) when Expression.is_operator(op) =>
      let is_a_next_word = (block, path: Path.t) =>
        switch (Path.next_word(block, path)) {
        | Some(_) => true
        | _ => false
        };
      let is_next_word_op = Path.is_next_word_p(Expression.word_is_operator);
      switch (current_path, Path.get_word(current_path, model.world)) {
      | (_, Some(op1)) when Expression.word_is_operator(op1) => model
      | (_, Some(op1)) when Word.is_empty(op1) =>
        app(UpdateWord(current_path, _ => op), model)
      | ([_, _, Word(Index(n, _)), ..._], _)
          when
            is_a_next_word(model.world, current_path)
            && !is_next_word_op(model.world, current_path) =>
        app(InsertWord(current_path, n + 1, Word.mk_name(op)), model)

      | ([_, _, Word(Index(n, _)), ..._], _) =>
        model
        |> app(InsertWord(current_path, n + 1, Word.mk_name(op)))
        |> app(InsertNewWord(current_path, n + 2))
      | _ => model
      };
    | InsertChar(x) =>
      let SingleCell(current_path) = model.focus;
      switch (Path.get_word(current_path, model.world), current_path) {
      | (Some(word), [_, _, Word(Index(n, _)), ..._])
          // if we're on an operator, advance to next word
          when Expression.word_is_operator(word) =>
        app(InsertWord(current_path, n + 1, Word.mk_name(x)), model)
      | _ =>
        app(UniFocus(UpdateWordF(n => Name.empty == n ? x : n ++ x)), model)
      };
    | Backspace =>
      let is_prev_word_op = Path.is_prev_word_p(Expression.word_is_operator);
      let remove_char = str => String.sub(str, 0, String.length(str) - 1);
      let words = Path.get_words(current_path, model.world);
      switch (current_path) {
      | [] when Block.len(model.world) != 0 =>
        let len = Block.len(model.world);
        app(SetFocus(SingleCell([Cell(Index(len - 1, len))])), model);
      | [Cell(Index(i, k))] =>
        let new_path = [Path.Cell(Index(i, k)), Field(Expression)];
        let length = Path.get_num_words(new_path, model.world);
        if (length == 0) {
          let new_path: Path.t = k == 1 ? [] : [Cell(Index(i - 1, k - 1))];
          model
          |> app(Delete(current_path))
          |> app(SetFocus(SingleCell(new_path)));
        } else {
          app(
            SetFocus(
              SingleCell(new_path @ [Word(Index(length - 1, length))]),
            ),
            model,
          );
        };
      | _ =>
        switch (words) {
        | Some([x]) when Word.is_empty(x) =>
          // if only empty word, delete cell
          Animate.cells(state);
          switch (current_path) {
          | [Cell(Index(0, l)), ..._] =>
            model
            |> app(Delete([Cell(Index(0, l))]))
            |> app(SetFocus(SingleCell([])))
          | [Cell(Index(cell_idx, l)), ..._] =>
            //TODO: select last word of cell
            let new_path: Path.t = [
              Cell(Index(cell_idx - 1, List.length(model.world) - 1)),
            ];
            model
            |> app(Delete([Cell(Index(cell_idx, l))]))
            |> app(SetFocus(SingleCell(new_path)));
          | _ => model
          };
        | _ =>
          /* Operators cannot be directly backspaced; if we try to
             backspace an empty word after an operator, we'll delete
             the operator. */
          switch (Path.get_word(current_path, model.world)) {
          | Some(word) when Expression.word_is_operator(word) =>
            app(UpdateWord(current_path, _ => Name.empty), model)
          | Some(word)
              when
                Word.is_empty(word)
                && is_prev_word_op(model.world, current_path) =>
            model
            |> app(UniFocus(DeleteF))
            |> app(SetFocus(SingleCell(Path.decr_word(current_path))))
            |> app(UniFocus(DeleteF))
            |> app(
                 SetFocus(
                   SingleCell(
                     current_path |> Path.decr_word |> Path.decr_word,
                   ),
                 ),
               )
          | Some(word) when Word.is_empty(word) =>
            model
            |> app(UniFocus(DeleteF))
            |> app(SetFocus(SingleCell(Path.decr_word(current_path))))
          | _ =>
            app(
              UniFocus(
                UpdateWordF(
                  n => String.length(n) == 1 ? Name.empty : remove_char(n),
                ),
              ),
              model,
            )
          }
        }
      };
    };
  };

//IDEA: dragging on numbers changes instead of moves?
