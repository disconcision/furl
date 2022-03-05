open Sexplib.Std;
open Util;
open Core;

[@deriving sexp]
type t =
  | SetFocus(Model.focus)
  | Pickup(Model.carry)
  | PickupTrash(int)
  | InsertWord(Path.t, Model.sep_id, Word.t)
  | InsertNewWord(Path.t, Model.sep_id)
  | InsertNewWordAfterFocus
  | UpdateWord(Path.t, Word.t => Word.t)
  | DropReplaceWord(Path.t)
  | UpdateFocusedWord(Word.t => Word.t)
  | SetDropTarget(Model.drop_target)
  | DropInsertWord(Path.t, Model.sep_id)
  | InsertCell(Block.cell_id, Cell.t)
  | InsertNewCell(Block.cell_id)
  | InsertNewCellAfterFocus
  | ReorderCell(Block.cell_id, int)
  | DropReorderCell(int)
  | SwapCells(Block.cell_id, Block.cell_id)
  | Delete(Path.t)
  | DeleteFocussed
  | DeleteCarryingSource
  | AddCarryToTrash((int, int))
  | EmptyTrash
  | DebugPrint
  | TogglePatternDisplay
  | UpdateKeymap(Model.keymap => Model.keymap);

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

let num_words_expression = (block, cell_idx) =>
  Block.nth_cell(block, cell_idx)
  |> ((x: Cell.t) => x.expression)
  |> List.length;

let is_only_word_empty_expression = (block, path, cell_idx) =>
  num_words_expression(block, cell_idx) == 1
  && Path.get_word(path, block) == Some(Word.empty);

let is_path_to_cell: Path.t => bool = path => List.length(path) == 1;

let is_path_to_word: Path.t => bool = path => List.length(path) == 3;

let rec apply: (Model.t, t, unit, ~schedule_action: 'a) => Model.t =
  (model: Model.t, update: t, state: State.t, ~schedule_action) => {
    //let model = update_drop_target(_ => NoTarget, model);
    let app = (a, m) => apply(m, a, state, ~schedule_action);
    let model =
      switch (update) {
      | SetFocus(focus) =>
        //TODO: check if previous focus is empty word, and if so delete it maybe?
        // do we distinguish between empty words and holes?
        update_focus(_ => focus, model)
      | Pickup(thing) => update_carry(_ => thing, model)
      | PickupTrash(idx) =>
        switch (List.nth(model.trash, idx)) {
        | TrashedWord(word, _) =>
          model
          |> update_trash(trash => ListUtil.remove(idx, trash))
          |> app(Pickup(WordBrush(word)))
        | TrashedCell(cell, _) =>
          model
          |> update_trash(trash => ListUtil.remove(idx, trash))
          |> app(Pickup(CellBrush(cell)))
        }
      | SetDropTarget(target) => update_drop_target(_ => target, model)
      | SwapCells(a, b) =>
        // TODO: refactored, not sure if works
        update_world(ListUtil.swap(a, b), model)
      | Delete(path) =>
        switch (path) {
        | [Cell(Index(cell_idx, _)), Field(Expression), _, ..._]
            when is_only_word_empty_expression(model.world, path, cell_idx) => model
        | [
            Cell(Index(cell_idx, _)),
            Field(Expression),
            Word(Index(word_idx, _)),
            ..._,
          ] =>
          update_world(
            world =>
              Block.update_expression(
                world,
                cell_idx,
                ListUtil.remove(word_idx),
              ),
            model,
          )
        | [Cell(Index(cell_idx, _))] =>
          update_world(ListUtil.remove(cell_idx), model)
        | _ => model
        }
      | DeleteFocussed =>
        switch (model.focus) {
        | SingleCell(path) => app(Delete(path), model)
        }
      | DeleteCarryingSource =>
        switch (model.carry) {
        | Nothing
        | WordBrush(_)
        | CellBrush(_) => model
        | Word(path)
        | Cell(path) => app(Delete(path), model)
        }
      | AddCarryToTrash((x, y)) =>
        switch (model.carry) {
        | Word(path) =>
          let word =
            switch (Path.get_word(path, model.world)) {
            | Some(word) => word
            | None => "lol. lmao."
            };
          model
          |> update_trash(trash => [TrashedWord(word, (x, y)), ...trash])
          |> app(DeleteCarryingSource);
        | Cell(path) =>
          let cell =
            switch (Path.get_cell(path, model.world)) {
            | Some(cell) => cell
            | None => Cell.init()
            };
          model
          |> update_trash(trash => [TrashedCell(cell, (x, y)), ...trash])
          |> app(DeleteCarryingSource);
        | _ => model
        }
      | EmptyTrash => update_trash(_ => [], model)
      | InsertCell(sep_idx, cell) =>
        //TODO: index 666
        model
        |> update_world(ListUtil.insert_at(sep_idx, cell))
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
      | InsertNewCellAfterFocus =>
        let SingleCell(current_path) = model.focus;
        switch (current_path) {
        | [Cell(Index(cell_idx, _)), ..._] =>
          app(InsertNewCell(cell_idx + 1), model)
        | _ => model
        };
      | DropReorderCell(new_idx) =>
        (
          switch (model.carry) {
          | Cell([Cell(Index(carry_idx, _)), ..._]) when model.keymap.shift =>
            let cell = Block.nth_cell(model.world, carry_idx);
            app(InsertCell(new_idx, cell), model);
          | Cell([Cell(Index(carry_idx, _)), ..._]) =>
            let cell = Block.nth_cell(model.world, carry_idx);
            let new_idx = new_idx > carry_idx ? new_idx - 1 : new_idx;
            model
            |> app(
                 Delete([
                   Cell(Index(carry_idx, List.length(model.world))),
                 ]),
               )
            |> app(InsertCell(new_idx, cell));
          | CellBrush(cell) => app(InsertCell(new_idx, cell), model)
          | _ => model
          }
        )
        // hack? sometimes ondragleave doesn't get triggered when dropping
        //|> update_drop_target(_ => NoTarget)
        |> app(Pickup(Nothing))
      | InsertWord(path, sep_idx, new_word) =>
        let m =
          update_world(
            world =>
              switch (path) {
              | [Cell(Index(cell_idx, _)), Field(Expression), ..._] =>
                Block.update_expression(
                  world,
                  cell_idx,
                  ListUtil.insert_at(sep_idx, new_word),
                )
              | _ => world
              },
            model,
          );
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
      | InsertNewWordAfterFocus =>
        let SingleCell(current_path) = model.focus;
        switch (current_path) {
        | [_, _, Word(Index(n, _)), ..._]
            when
              Path.get_word(current_path, model.world)
              != Some(Core.Word.empty) =>
          app(InsertNewWord(current_path, n + 1), model)
        | _ => model
        };
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
      | UpdateFocusedWord(f) =>
        switch (model.focus) {
        | SingleCell(path) => app(UpdateWord(path, f), model)
        }
      | UpdateWord(path, f) =>
        update_world(
          world =>
            switch (path) {
            | [
                Cell(Index(cell_idx, _)),
                Field(Expression),
                Word(Index(word_idx, _)),
                ..._,
              ] =>
              Block.update_expression(
                world,
                cell_idx,
                Cell.update_word(f, word_idx),
              )
            | _ => world
            },
          model,
        )
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
      | DebugPrint =>
        let ann_block = AnnotatedBlock.mk(model.world);
        print_endline(
          Sexplib.Sexp.to_string_hum(
            AnnotatedBlock.sexp_of_annotated_block(ann_block),
          ),
        );
        print_endline("FURLBLOCK:");
        print_endline(
          Sexplib.Sexp.to_string_hum(
            Expression.sexp_of_form(FurledBlock.furl_block(ann_block)),
          ),
        );
        print_endline("FOCUS:");
        print_endline(
          Sexplib.Sexp.to_string_hum(Model.sexp_of_focus(model.focus)),
        );
        model;
      | TogglePatternDisplay =>
        update_pattern_display(
          x =>
            switch (x) {
            | Name => Emoji
            | Emoji => Name
            },
          model,
        )
      | UpdateKeymap(f) => update_keymap(f, model)
      };
    update_world(Interpreter.run_block, model);
  };

//IDEA: dragging on numbers changes instead of moves?
