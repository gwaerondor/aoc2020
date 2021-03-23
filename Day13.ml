open Core

type schedule = Schedule of int * int list

let parse_input timestamp departures =
  let time = int_of_string timestamp in
  let buslines =
    AocLib.csv_of_line departures
    |> List.filter ~f:(fun c -> not (String.equal c "x"))
    |> List.map ~f:int_of_string
  in
  Schedule (time, buslines)

let input =
  let lines = AocLib.lines_of_file "input/13" in
  match lines with
  | (x :: y :: []) -> parse_input x y
  | _ -> AocLib.halt "Couldn't parse input"

let rec find_first ~pred = function
  | (x :: rest) -> if (pred x) then Some x else find_first ~pred:pred rest
  | [] -> None

let divides x y = (x mod y) = 0

let exec_1 =
  match input with
  | Schedule (earliest, departures) ->
     let rec find_next_departure ~time =
       match find_first ~pred:(divides time) departures with
       | Some id -> id * (time - earliest)
       | None -> find_next_departure ~time:(time + 1)
     in
     find_next_departure ~time:earliest

let exec_2 = "?"

let main =
  let s1 = exec_1 |> string_of_int in
  let s2 = exec_2 in
  AocLib.print_two_part_solution s1 s2
