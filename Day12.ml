open Core

type facing = North | East | South | West | Left | Right | Forward
type action =
  Action of facing * int

let to_action s =
  let c2i n = (String.of_char_list n |> Int.of_string) in
  match String.to_list s with
  | ('N' :: n) -> Action (North, (c2i n))
  | ('E' :: n) -> Action (East, (c2i n))
  | ('S' :: n) -> Action (South, (c2i n))
  | ('W' :: n) -> Action (West, (c2i n))
  | ('L' :: n) -> Action (Left, (c2i n))
  | ('R' :: n) -> Action (Right, (c2i n))
  | ('F' :: n) -> Action (Forward, (c2i n))
  | _ -> AocLib.halt "Invalid input"

let input = AocLib.lines_of_file_map ~f:to_action "input/12"

let rec turn ~facing ~deg =
  match deg with
  | 0 -> facing
  | _ ->
     let next_deg = deg - 90 in
     let next_facing = match facing with
       | North -> East
       | East -> South
       | South -> West
       | West -> North
       | _ -> facing
     in
     turn ~facing:next_facing ~deg:next_deg

let rec exec_1 ~ship ~facing ~input =
  let (x, y) = ship in
  match input with
  | [] -> (abs x) + (abs y)
  | (i :: rest) ->
     match i with
     | Action (North, n) -> exec_1 ~ship:(x, y + n) ~facing:facing ~input:rest
     | Action (East, n) -> exec_1 ~ship:(x + n, y) ~facing:facing ~input:rest
     | Action (South, n) -> exec_1 ~ship:(x, y - n) ~facing:facing ~input:rest
     | Action (West, n) -> exec_1 ~ship:(x - n, y) ~facing:facing ~input:rest
     | Action (Left, deg) -> exec_1 ~ship:(x, y) ~facing:(turn ~facing:facing ~deg:(360 - deg)) ~input:rest
     | Action (Right, deg) -> exec_1 ~ship:(x, y) ~facing:(turn ~facing:facing ~deg:deg) ~input:rest
     | Action (Forward, steps) ->
        let (new_x, new_y) = match facing with
          | North -> (x, y + steps)
          | East -> (x + steps, y)
          | South -> (x, y - steps)
          | West -> (x - steps, y)
          | _ -> AocLib.halt "Generic error message."
        in
        exec_1 ~ship:(new_x, new_y) ~facing:facing ~input:rest

let rec transpose ~waypoint:(x, y) ~deg =
  match deg with
  | 0 -> (x, y)
  | _ -> transpose ~waypoint:(y, (-x)) ~deg:(deg - 90)

let rec exec_2 ~ship ~waypoint ~input =
  let (sx, sy) = ship in
  let (wx, wy) = waypoint in
  match input with
  | [] -> (abs sx) + (abs sy)
  | (i :: rest) ->
     match i with
     | Action (North, n) -> exec_2 ~ship:ship ~waypoint:(wx, wy + n) ~input:rest
     | Action (East, n) -> exec_2 ~ship:ship ~waypoint:(wx + n, wy) ~input:rest
     | Action (South, n) -> exec_2 ~ship:ship ~waypoint:(wx, wy - n) ~input:rest
     | Action (West, n) -> exec_2 ~ship:ship ~waypoint:(wx - n, wy) ~input:rest
     | Action (Left, deg) -> exec_2 ~ship:ship ~waypoint:(transpose ~waypoint:waypoint ~deg:(360 - deg)) ~input:rest
     | Action (Right, deg) -> exec_2 ~ship:ship ~waypoint:(transpose ~waypoint:waypoint ~deg:deg) ~input:rest
     | Action (Forward, steps) -> exec_2 ~ship:(sx + (wx * steps), sy + (wy * steps)) ~waypoint:waypoint ~input:rest

let main =
  let s1 = exec_1 ~ship:(0, 0) ~facing:East ~input:input in
  let s2 = exec_2 ~ship:(0, 0) ~waypoint:(10, 1) ~input:input in
  AocLib.print_two_part_solution (s1 |> Int.to_string) (s2 |> Int.to_string)
