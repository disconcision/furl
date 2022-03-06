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
  | UpdateWord(Word.t => Word.t)
  | DeleteF
  | InsertNewWordF
  | InsertNewCellF
  | Backspace;

[@deriving sexp]
type t =
  | Animtest(bool)
  | SetFocus(Model.focus)
  | UniFocus(single_focus_action)
  | Delete(Path.t)
  | TogglePatternDisplay
  | UpdateKeymap(Model.keymap => Model.keymap)
  | DebugPrint
  // words
  | UpdateWord(Path.t, Word.t => Word.t)
  | InsertWord(Path.t, Model.word_sep_id, Word.t)
  | InsertNewWord(Path.t, Model.word_sep_id)
  // cells
  | InsertCell(Block.cell_id, Cell.t)
  | InsertNewCell(Block.cell_id)
  | ReorderCell(Block.cell_id, Model.cell_sep_id)
  | SwapCells(Block.cell_id, Block.cell_id)
  // drag-n-drop
  | Pickup(Model.carry)
  | DropReplaceWord(Path.t)
  | SetDropTarget(Model.drop_target)
  | DropInsertWord(Path.t, Model.word_sep_id)
  | DropReorderCell(Model.cell_sep_id)
  | DeleteCarrySource
  // trash
  | PickupTrash(int)
  | AddCarryToTrash((int, int))
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

let rec apply: (Model.t, t, unit, ~schedule_action: 'a) => Model.t =
  (model: Model.t, update: t, state: State.t, ~schedule_action) => {
    //let model = update_drop_target(_ => NoTarget, model);
    let app = (a, m) => apply(m, a, state, ~schedule_action);
    let model =
      switch (update) {
      | Animtest(_b) => {...model, animtest: !model.animtest}
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
          |> app(Pickup(WordBrush(word)))
        | TrashedCell(cell, _) =>
          model
          |> update_trash(ListUtil.remove(idx))
          |> app(Pickup(CellBrush(cell)))
        }
      | DeleteCarrySource =>
        switch (model.carry) {
        | Nothing
        | WordBrush(_)
        | CellBrush(_) => model
        | Word(path)
        | Cell(path) => app(Delete(path), model)
        }
      | AddCarryToTrash(coords) =>
        switch (model.carry) {
        | Word(path) =>
          let word =
            switch (Path.get_word(path, model.world)) {
            | Some(word) => word
            | None => "lol. lmao."
            };
          model
          |> update_trash(trash => [TrashedWord(word, coords), ...trash])
          |> app(DeleteCarrySource);
        | Cell(path) =>
          let cell =
            switch (Path.get_cell(path, model.world)) {
            | Some(cell) => cell
            | None => Cell.init()
            };
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
        app(InsertCell(sep_idx, Core.Cell.init()), model)
      | ReorderCell(cell_idx, new_idx) =>
        let cell = Block.nth_cell(model.world, cell_idx);
        let new_idx = new_idx > cell_idx ? new_idx - 1 : new_idx;
        model
        |> app(Delete([Cell(Index(cell_idx, List.length(model.world)))]))
        |> app(InsertCell(new_idx, cell));
      | DropReorderCell(new_idx) =>
        let block = model.world;
        (
          switch (model.carry) {
          | Cell([Cell(Index(carry_idx, _)), ..._]) when model.keymap.shift =>
            let cell = Block.nth_cell(block, carry_idx);
            app(InsertCell(new_idx, cell), model);
          | Cell([Cell(Index(carry_idx, _)), ..._]) =>
            let cell = Block.nth_cell(block, carry_idx);
            let new_idx = new_idx > carry_idx ? new_idx - 1 : new_idx;
            model
            |> app(Delete([Cell(Index(carry_idx, List.length(block)))]))
            |> app(InsertCell(new_idx, cell));
          | CellBrush(cell) => app(InsertCell(new_idx, cell), model)
          | _ => model
          }
        )
        |> app(Pickup(Nothing));
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
        app(InsertWord(path, sep_idx, Core.Word.empty), model)
      | DropInsertWord(path, sep_idx) =>
        (
          switch (model.carry) {
          | Word([_, _, Word(Index(word_idx, _)), ..._] as word_path) =>
            switch (Core.Path.get_word(word_path, model.world)) {
            | None => model
            | Some(carry_word) when model.keymap.shift =>
              // if holding shift, copy instead of move
              app(InsertWord(path, sep_idx, carry_word), model)
            | Some(carry_word) =>
              if (sep_idx > word_idx) {
                model
                |> app(InsertWord(path, sep_idx, carry_word))
                |> app(Delete(word_path));
              } else {
                model
                |> app(Delete(word_path))
                |> app(InsertWord(path, sep_idx, carry_word));
              }
            }
          | WordBrush(word) => app(InsertWord(path, sep_idx, word), model)
          | _ => model
          }
        )
        // hack? sometimes ondragleave doesn't get triggered when dropping
        //|> update_drop_target(_ => NoTarget)
        |> app(Pickup(Nothing))
      | DropReplaceWord(path) =>
        // TODO: figure out how to combine this with dropinsertword
        switch (path) {
        | [_, _, Word(Index(path_word_idx, _)), ..._] =>
          (
            switch (model.carry) {
            | Word([_, _, Word(Index(word_idx, _)), ..._] as word_path) =>
              switch (Core.Path.get_word(word_path, model.world)) {
              | None => model
              | Some(carry_word) when model.keymap.shift =>
                // if holding shift, copy instead of move
                model
                |> app(Delete(path))
                |> app(InsertWord(path, path_word_idx, carry_word))
              | Some(_) when path_word_idx == word_idx => model
              | Some(carry_word) =>
                if (path_word_idx > word_idx) {
                  model
                  |> app(InsertWord(path, path_word_idx, carry_word))
                  |> app(Delete(word_path))
                  |> app(Delete(path));
                } else {
                  model
                  |> app(Delete(word_path))
                  |> app(Delete(path))
                  |> app(InsertWord(path, path_word_idx, carry_word));
                }
              }
            | WordBrush(word) =>
              model
              |> app(Delete(path))
              |> app(InsertWord(path, path_word_idx, word))
            | _ => model
            }
          )
          |> app(Pickup(Nothing))
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
      | DebugPrint =>
        let ann_block = AnnotatedBlock.mk(model.world);
        let furled = FurledBlock.furl_block(ann_block);
        Util.P.ps([
          ("FOCUS:", Model.sexp_of_focus(model.focus)),
          ("BLOCK:", Block.sexp_of_t(model.world)),
          ("ANN:", AnnotatedBlock.sexp_of_annotated_block(ann_block)),
          ("FURLED:", Expression.sexp_of_form(furled)),
        ]);
        model;
      };
    update_world(Interpreter.run_block, model);
  }

and apply_single:
  (single_focus_action, Model.t, unit, ~schedule_action: 'a) => Model.t =
  (a, model, state, ~schedule_action) => {
    let SingleCell(current_path) = model.focus;
    let app = (a, m) => apply(m, a, state, ~schedule_action);
    let update_focus = (f: (Block.t, Path.t) => Path.t, m: Model.t) => {
      app(SetFocus(SingleCell(f(m.world, current_path))), m);
    };
    switch (a) {
    | UpdateWord(f) => app(UpdateWord(current_path, f), model)
    | DeleteF =>
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
            Path.get_word(current_path, model.world) != Some(Core.Word.empty) =>
        app(InsertNewWord(current_path, n + 1), model)
      | _ => model
      };
    | SwapCellDown =>
      switch (current_path) {
      | [Cell(Index(cell_idx, k))] when cell_idx != k - 1 =>
        model
        |> app(ReorderCell(cell_idx + 1, cell_idx))
        |> app(SetFocus(SingleCell([Cell(Index(cell_idx + 1, k))])))
      | _ => model
      }
    | SwapCellUp =>
      switch (current_path) {
      | [Cell(Index(cell_idx, k))] when cell_idx != 0 =>
        model
        |> app(ReorderCell(cell_idx, cell_idx - 1))
        |> app(SetFocus(SingleCell([Cell(Index(cell_idx - 1, k))])))
      | _ => model
      }
    | MoveDown => update_focus(Path.down_path, model)
    | MoveUp => update_focus(Path.up_path, model)
    | MoveRight => update_focus(Path.next_word_path, model)
    | MoveLeft => update_focus(Path.prev_word_path, model)
    | InsertChar(op) when Expression.is_operator(op) =>
      let is_a_next_word = (block, path: Path.t) =>
        switch (Path.next_word(block, path)) {
        | Some(_) => true
        | _ => false
        };
      let is_next_word_op = Path.is_next_word_p(Expression.is_operator);
      switch (current_path, Path.get_word(current_path, model.world)) {
      | (_, Some(op1)) when Expression.is_operator(op1) => model
      | (_, Some(op1)) when op1 == Word.empty =>
        app(UpdateWord(current_path, _ => op), model)
      | ([_, _, Word(Index(n, _)), ..._], _)
          when
            is_a_next_word(model.world, current_path)
            && !is_next_word_op(model.world, current_path) =>
        app(InsertWord(current_path, n + 1, op), model)

      | ([_, _, Word(Index(n, _)), ..._], _) =>
        model
        |> app(InsertWord(current_path, n + 1, op))
        |> app(InsertNewWord(current_path, n + 2))
      | _ => model
      };
    | InsertChar(x) =>
      let SingleCell(current_path) = model.focus;
      switch (Path.get_word(current_path, model.world), current_path) {
      | (Some(word), [_, _, Word(Index(n, _)), ..._])
          // if we're on an operator, advance to next word
          when Expression.is_operator(word) =>
        app(InsertWord(current_path, n + 1, x), model)
      | _ =>
        app(UniFocus(UpdateWord(w => w == Word.empty ? x : w ++ x)), model)
      };
    | Backspace =>
      let is_prev_word_op = Path.is_prev_word_p(Expression.is_operator);
      let remove_char = str => String.sub(str, 0, String.length(str) - 1);
      let words = Path.get_words(current_path, model.world);
      switch (current_path) {
      | [Cell(Index(i, k))] =>
        let new_path = [Path.Cell(Index(i, k)), Field(Expression)];
        let length = Path.get_num_words(new_path, model.world);
        app(
          SetFocus(
            SingleCell(new_path @ [Word(Index(length - 1, length))]),
          ),
          model,
        );
      | _ =>
        switch (words) {
        | Some([x]) when x == Word.empty =>
          // if only empty word, delete cell
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
          }
        | _ =>
          /* Operators cannot be directly backspaced; if we try to
             backspace an empty word after an operator, we'll delete
             the operator. */
          switch (Path.get_word(current_path, model.world)) {
          | Some(word) when Expression.is_operator(word) =>
            app(UpdateWord(current_path, _ => Word.empty), model)
          | Some(word)
              when
                word == Word.empty
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
          | Some(word) when word == Word.empty =>
            model
            |> app(UniFocus(DeleteF))
            |> app(SetFocus(SingleCell(Path.decr_word(current_path))))
          | _ =>
            app(
              UniFocus(
                UpdateWord(
                  w => String.length(w) == 1 ? Word.empty : remove_char(w),
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
