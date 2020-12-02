open Core

let print_string s =
  Out_channel.output_string stdout s;
  Out_channel.newline stdout

type group =
  | Pair of int * int
  | Trio of int * int * int

let lines = AocLib.lines_of_file_int "input/01"
let correctSum = function
  | Pair (x, y) -> (x + y) = 2020
  | Trio (x, y, z) -> (x + y + z) = 2020

let main =
  let rec check h elements =
    match elements with
    | (x::_) when correctSum (Pair (x, h)) ->
       print_string (string_of_int (x * h));
       exit 0
    | (_::rest) ->
       check h rest
    | _ ->
       ()
  in
  List.map ~f:(fun h -> check h lines) lines
