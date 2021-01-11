open Core

let preamble_length = 25

let input = AocLib.lines_of_file_map ~f:(Int.of_string) "input/09"

let rec tails ls =
  match ls with
  | x :: rest -> (x :: rest) :: (tails rest)
  | [] -> []

let pairs ls =
  let lss = tails ls in
  let rec pair_up xs =
    match xs with
    | x :: rest -> (List.map ~f:(fun y -> (x, y)) rest) :: (pair_up rest)
    | [] -> []
  in List.map ~f:pair_up lss |> List.concat |> List.concat

let is_valid ~v ~preamble =
  let prev_combinations = pairs preamble in
  List.exists ~f:(fun (p1, p2) -> (p1 + p2) = v) prev_combinations

let rec solution_1 ~input =
  let pre = List.take input preamble_length in
  let current = match List.nth input preamble_length with
    | Some x -> x
    | None -> AocLib.halt "didn't find any applicable number"
  in
  match is_valid ~v:current ~preamble:pre with
  | false -> current
  | true -> solution_1 ~input:(List.drop input 1)

let smallest ls = match List.min_elt ~compare:Int.compare ls with
  | Some x -> x
  | None -> AocLib.halt "empty list"

let largest ls = match List.max_elt ~compare:Int.compare ls with
  | Some x -> x
  | None -> AocLib.halt "empty list"

let rec solution_2 ~input ~target =
  let rec check_for_target (hd::rem) curr =
    let sum = List.fold_left ~f:(+) ~init:0 curr in
    if sum > target then None
    else if sum = target then Some (smallest curr, largest curr)
    else check_for_target rem (hd :: curr)
  in
  let (i :: rest) = input in
  match check_for_target rest [i] with
  | Some (x, y) -> x + y
  | None ->
     solution_2 ~input:rest ~target:target

let main =
  let s1 = (solution_1 ~input:input) in
  AocLib.print_two_part_solution
    (Int.to_string s1)
    (solution_2 ~input:input ~target:s1 |> Int.to_string)
