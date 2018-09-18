open Base
open Virtual_dom.Vdom

let fetch url on_done =
  let xhr = XmlHttpRequest.create () in
  xhr##_open (Js.string "GET") (Js.string url) (Js.bool true);
  xhr##setRequestHeader (Js.string "Accept") (Js.string "application/json");
  xhr##.onreadystatechange := Js.wrap_callback (fun () ->
      match xhr##.readyState with
      | XmlHttpRequest.DONE ->
        on_done xhr##.status (Js.to_string xhr##.responseText)
      | _ ->
        ()
    );
  xhr##send Js.null

let confirm ?(question="Are you sure?") f =
  if Js.to_bool (Dom_html.window##confirm (Js.string question)) then
    f ()
  else
    Event.Ignore

module Counter = struct
  type model =
    { counter : int
    }

  let model_of_int n =
    { counter = n }

  let model =
    { counter = 0 }

  let update model action =
    let diff = match action with
      | `Increment -> 1
      | `Decrement -> -1
    in
    { counter = model.counter + diff }

  let view inject model =
    let open Node in
    let open Attr in

    let butt action txt =
      button [ on_click (fun _ -> inject action)
             ; style [ "padding", "8px"
                     ; "border-radius", "50%"
                     ; "border", "2px solid blue"
                     ]
             ]
        [text txt]
    in

    div [style ["display", "flex"; "flex-direction", "row"; "align-items", "center"]]
      [ butt `Increment "+"
      ; div [style ["font-size", "24px"; "margin", "0 8px"]]
          [text (Int.to_string model.counter)]
      ; butt `Decrement "-"
      ]
end

module CounterList = struct
  type model =
    Counter.model list

  let model =
    []

  let model_of_list xs =
    List.map ~f:Counter.model_of_int xs

  let update model = function
    | `Add ->
      Counter.model :: model
    | `Counter (i, a) ->
      List.mapi ~f:(fun j m -> if i = j then Counter.update m a else m) model
    | `Remove i ->
      List.take model i @ List.drop model (i + 1)

  let view inject model =
    let open Node in
    let open Attr in
    let view_counter i model =
      div [ style [ "display", "flex"
                  ; "flex-direction", "row"
                  ; "padding-bottom", "8px"
                  ; "margin-bottom", "8px"
                  ; "border-bottom", "1px solid #eee"
                  ]
          ]
        [ Counter.view (fun a -> inject (`Counter (i, a))) model
        ; div [style ["flex", "1"; "text-align", "right"]]
            [button [ on_click (fun _ -> confirm (fun () -> inject (`Remove i)))
                    ; style ["align-self", "center"]
                    ]
               [text "×"]
            ]
        ]
    in
    div []
      [ button [ on_click (fun _ -> inject `Add)
               ; style [ "margin", "0 0 12px 0"
                       ; "padding", "8px 12px"
                       ; "border-radius", "4px"
                       ; "border", "2px solid black"
                       ; "font-size", "16px"
                       ]
               ]
          [text "+ Add New Counter"]
      ; div []
          (List.mapi ~f:view_counter model)
      ]
end

module App = struct
  exception Impossible_action_for_model

  type model =
    | Wait
    | Downloading
    | Done of CounterList.model

  let model =
    Wait

  type 'a action =
    | Fetch of string
    | Receive of int list
    | CounterList of 'a

  let update model = function
    | Fetch url ->
      Downloading
    | Receive counters ->
      Done (CounterList.model_of_list counters)
    | CounterList action ->
      match model with
      | Done model -> Done (CounterList.update model action)
      | _ -> raise Impossible_action_for_model

  let view inject model =
    let open Node in
    let open Attr in
    match model with
    | Wait ->
      div [] [text "Warming up..."]
    | Downloading ->
      div [] [text "Loading..."]
    | Done counters ->
      div [] [ h1 [] [text "Import Number List"]
             ; CounterList.view (fun action -> inject (CounterList action)) counters
             ; button [] [text "Reload"]
             ]
end

let () =
  Elm.on_dom_load (fun () ->
      let el = Dom_html.getElementById "app" in
      let inject = Elm.start_app ~model:App.model ~view:App.view ~update:App.update el in
      inject (App.Receive [3;4;5;8;2;4;9])
      (* fetch "counters.json" (fun status response_text ->
       *     let numbers = Ezjsonm.(from_string response_text |> get_list get_int) in
       *     inject (App.Receive numbers)
       *   ) *)
    )
