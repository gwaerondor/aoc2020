open Core

let input =
  let splitter = Re2.split (Re2.create_exn "\n\n") in
  AocLib.contents_of_file_map ~f:splitter "input/06"

let set_size ls =
  String.to_list ls
  |> List.dedup_and_sort ~compare:Char.compare
  |> List.length

let solution_1 =
  List.map ~f:(fun x -> (String.split ~on:'\n' x |> String.concat)) input
  |> List.map ~f:set_size
  |> List.fold_left ~f:(+) ~init:0
  |> Int.to_string

let main =
  AocLib.print_two_part_solution solution_1 "?"
