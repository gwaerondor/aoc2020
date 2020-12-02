open Core

let lines_of_file_map ~f filename =
  In_channel.read_lines filename |> List.map ~f:f

let lines_of_file_int = lines_of_file_map ~f:int_of_string

let print_strings =
  List.map ~f:(Out_channel.output_string stdout)
