open Base
open Opium.Std

type counters = int list

let numbers = [1;2;3;4;9;0;1]

let list_numbers = get "/counters" begin fun _ ->
    let json = Ezjsonm.(list int numbers) in
    `Json json |> respond'
end

let _ =
  App.empty
  |> list_numbers
  |> App.run_command
