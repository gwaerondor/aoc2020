open Core

let parse_seat spec =
  let row_spec = String.prefix spec 7
                 |> String.map ~f:(function | 'F' -> '0'
                                            | 'B' -> '1')
  in
  let col_spec = String.suffix spec 3
                 |> String.map ~f:(function | 'L' -> '0'
                                            | 'R' -> '1')
  in
  let row = Int.of_string ("0b" ^ row_spec) in
  let col = Int.of_string ("0b" ^ col_spec) in
  (row, col)

let input = AocLib.lines_of_file_map ~f:(parse_seat) "input/05"
let seat_hash (row, col) = (row * 8) + col

let rec find_first_missing_in_sequence xs =
  match xs with
  | (x::y::rest) when x + 1 = y -> find_first_missing_in_sequence (y::rest)
  | (x::_) -> x + 1

let solution_1 =
  List.map ~f:seat_hash input
  |> List.fold_left ~f:max ~init:0
  |> Int.to_string

let solution_2 =
  List.map ~f:seat_hash input
  |> List.sort ~compare:Int.compare
  |> find_first_missing_in_sequence
  |> Int.to_string

let main =
  AocLib.print_two_part_solution solution_1 solution_2
