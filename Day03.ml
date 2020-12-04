open Core

type topology = Tree | Open
let to_topology = function
  | '#' -> Tree
  | _ -> Open

let parse_map line = String.to_list line |> List.map ~f:to_topology
let input = AocLib.lines_of_file_map ~f:parse_map "input/03"

let rec traverse map x ~step_x ~step_y ~acc =
  match map with
  | [] -> acc
  | (row :: rest) ->
     let actual_x = (x mod (List.length row)) in
     let next = List.drop rest (step_y - 1) in
     let trees_hit = match List.nth row actual_x with
       | Some Tree -> 1
       | Some Open -> 0
       | None -> AocLib.halt (String.concat ["Index out of bounds: ";
                                             (actual_x |> Int.to_string)])
     in
     traverse next (x + step_x) ~step_x:step_x ~step_y:step_y ~acc:(acc + trees_hit)

let do_it (x, y) = traverse input 0 ~step_x:x ~step_y:y ~acc:0

let part_1 = do_it (3, 1) |> Int.to_string
let part_2 =
  List.map ~f:do_it [(1, 1); (3, 1); (5, 1); (7, 1); (1, 2)]
  |> List.fold_left ~f:( * ) ~init:1
  |> Int.to_string

let main =
  AocLib.print_two_part_solution part_1 part_2

