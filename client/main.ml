open Base
open Virtual_dom.Vdom

exception Impossible_state

type model =
  | Level of {level: Level.t; counter: Counter.t}
  | Playing of {level: Level.t; counter: Counter.t; puzzle: Puzzle.t}
[@@deriving json]

type action =
  | Play
  | Quit
  | Level_action of Level.action
  | Counter_action of Counter.action

let init =
  Level {level = Level.init; counter = Counter.init}

let start_game level =
  let (size, timeout) = Level.to_size_and_timeout level in
  Puzzle.init ~size ~timeout "images/01.jpg" |> Puzzle.shuffle

let update model action =
  match action, model with
  | Play, Level {level; counter} ->
    Playing {level; counter; puzzle = start_game level}

  | Quit, Playing {level; counter} ->
    Level {level; counter}

  | Level_action action, Level {level; counter} ->
    Level {level = Level.update level action; counter}

  | Counter_action action, Level {level; counter} ->
    Level {level; counter = Counter.update counter action}

  | _ ->
    raise Impossible_state

let button =
  Vdom.elt "button"

let view model =
  match model with
  | Level {level = current_level; counter} ->
    div [ Vdom.map (fun action -> Level_action action)
            (Level.view current_level)
        ; button ~a:[onclick (Fn.const Play)] [text "Start Game"]
        ; elt "hr" []
        ; Vdom.map (fun action -> Counter_action action)
            (Counter.view counter)
        ]

  | Playing {puzzle} ->
    div [ button ~a:[onclick (Fn.const Quit)] [text "Quit"]
        ; Puzzle.view puzzle
        ]

let cache_model update_fn =
  fun model action ->
    let new_model = update_fn model action in
    Option.iter (Window.local_storage window) ~f:(fun st ->
        Storage.set_item st "model" (Deriving_Json.to_string model_json new_model)
      );
    new_model

let from_cache () =
  Option.(
    Window.local_storage window
    >>= (fun storage -> Storage.get_item storage "model")
    >>= (fun model_str ->
        Option.try_with
          (fun () -> Deriving_Json.from_string model_json model_str))
    |> Option.value ~default:init
  )

let dev_app =
  simple_app ~init:(from_cache ()) ~view ~update:(cache_model update) ()

let app =
  simple_app ~init ~view ~update ()

let run () =
  Vdom_blit.run dev_app
  |> Vdom_blit.dom
  |> Element.append_child (Document.body document)

let () =
  Window.set_onload window run
