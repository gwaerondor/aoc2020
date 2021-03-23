open Core

let input =
  let splitter = Re2.split (Re2.create_exn "\n\n") in
  AocLib.contents_of_file_map ~f:splitter "input/06"
  |> List.map ~f:(fun x -> (String.split ~on:'\n' x))

let set_size ls =
  String.to_list ls
  |> List.dedup_and_sort ~compare:Char.compare
  |> List.length

let solution_1 =
  List.map ~f:(String.concat ~sep:"") input
  |> List.map ~f:set_size
  |> List.fold_left ~f:(+) ~init:0
  |> Int.to_string

let intersect a b =
  let m = List.mem ~equal:Char.equal b in
  List.filter ~f:m a

let intersect_strings a b =
  intersect (String.to_list a) (String.to_list b)
  |> String.of_char_list

let intersect_many = function
  | (str :: []) -> str
  | (str :: strs) -> List.fold_left ~f:intersect_strings ~init:str strs
  | [] -> ""

let solution_2 =
  let groups = List.map ~f:intersect_many input in
  let amount = List.map ~f:String.length groups
               |> List.fold_left ~f:(+) ~init: 0
               |> Int.to_string
  in
  let _ = AocLib.print_strings ~separator:"\n" (List.map ~f:(fun s -> ("[" ^ (String.length s |> Int.to_string) ^ ": " ^ s ^ "]")) groups) in
  amount

let main =
  AocLib.print_two_part_solution solution_1 solution_2
