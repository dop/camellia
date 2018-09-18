open Base
open Virtual_dom.Vdom

type t =
  | Hard
  | Advanced
  | Easy
[@@deriving json, compare]

let to_size_and_timeout = function
  | Easy -> (3, 60)
  | Advanced -> (4, 180)
  | Hard -> (5, 300)

let to_string = function
  | Easy -> "Easy"
  | Advanced -> "Advanced"
  | Hard -> "Hard"

type action =
  | Select_level of t

let init =
  Easy

let update model action =
  match action with
  | Select_level level ->
    level

let level_equals a b =
  compare a b = 1

let empty_attr =
  Attr.create "" ""

let view_level_input current_level level name =
  let open Node in
  let open Attr in
  div
    []
    [ label []
        [ input
            [ type_ "radio"
            ; create "name" "level"
            ; on_click (fun evt -> Event.Ignore)
            ; if level_equals current_level level
              then checked
              else empty_attr
            ]
            []
        ; text name
        ]
    ]

let view current_level =
  let open Node in
  let open Attr in
  div []
    [div []
       [ h1 [] [text "Choose difficulty"]
       ; view_level_input current_level Easy "Easy"
       ; view_level_input current_level Advanced "Advanced"
       ; view_level_input current_level Hard "Hard"
       ]
    ]
