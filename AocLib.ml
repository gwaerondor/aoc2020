open Core

let contents_of_file_map ~f filename =
  In_channel.read_all filename |> f

let lines_of_file_map ~f filename =
  In_channel.read_lines filename |> List.map ~f:f

let lines_of_file_int = lines_of_file_map ~f:int_of_string

let csv_of_file filename =
  let lines = In_channel.read_lines filename in
  match List.hd lines with
  | Some x -> String.split_on_chars ~on:[','] x
  | None -> []

let print_strings =
  List.map ~f:(Out_channel.output_string stdout)

let print_two_part_solution p1 p2 =
  print_strings ["Part 1: "; p1; "\n";
                 "Part 2: "; p2; "\n"]

let list_length_str ls = List.length ls |> Int.to_string

let halt error_message =
  Out_channel.output_string stderr error_message;
  exit 1
