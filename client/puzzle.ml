open Base
open Vdom

let on g f a b =
  g (f a) (f b)

let compare_on f =
  on compare f

let shuffle_list xs =
  List.map ~f:(fun i -> (i, Random.bits ())) xs
  |> List.sort ~compare:(compare_on snd)
  |> List.map ~f:fst

module Board = struct
  exception Invalid_size

  type t =
    { size : int
    ; tiles : int list
    }
  [@@deriving json]

  let init size =
    if size > 0 then
      { size
      ; tiles = List.init (size * size) ~f:Fn.id
      }
    else
      raise Invalid_size

  let shuffle board =
    { board with tiles = shuffle_list board.tiles }

  let tiles board =
    board.tiles

  let size board =
    board.size
end

type t =
  { board : Board.t
  ; image : string
  ; current_time : int
  ; timeout : int
  }
[@@deriving json]

type action =
  | Start_drag of int
  | Drag of {tile : int; x : int; y : int}
  | Stop_drag of int

let init image ~size ~timeout =
  { board = Board.init size
  ; current_time = 0
  ; image
  ; timeout
  }

let shuffle puzzle =
  { puzzle with board = Board.shuffle puzzle.board }

let view {image; board} =
  let size_to_fill = 500 in
  let size = Board.size board in
  let tile_width = size_to_fill / size in
  let board_width = tile_width * size in
  let px num = Int.to_string num ^ "px" in
  let tile index =
    div ~a:[ style "overflow" "hidden"
           ; style "position" "absolute"
           ; style "width" (px tile_width)
           ; style "height" (px tile_width)
           ; style "left" (px ((index % size) * tile_width))
           ; style "top" (px ((index / size) * tile_width))
           ; onmouseup (fun _ -> Start_drag index)
           ; onmousedown (fun _ -> Stop_drag index)
           ]
      [ elt "img" [] ~a:[ attr "src" image
                        ; style "display" "block"
                        ; style "position" "absolute"
                        ; style "width" (px board_width)
                        ; style "height" (px board_width)
                        ; style "left" (px (-(index % size) * tile_width))
                        ; style "top" (px (-(index / size) * tile_width))
                        ]
      ]
  in
  div ~a:[ style "position" "relative"
         ; style "width" (px board_width)
         ; style "height" (px board_width)
         ]
    (List.map ~f:tile (Board.tiles board))
