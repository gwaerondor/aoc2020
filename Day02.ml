open Core

type rule = Rule of char * int * int
type password = Password of string * rule

let parse_input text =
  let (r, p) = String.lsplit2_exn ~on:':' text in
  let pw = String.strip p in
  let (amounts, char) = String.lsplit2_exn ~on:' ' r in
  let (fr, t) = String.lsplit2_exn ~on:'-' amounts in
  let rule = Rule (Char.of_string char, Int.of_string fr, Int.of_string t) in
  Password (pw, rule)

let is_within ~value ~low ~high = (value >= low) && (value <= high)

let is_valid = function
  | Password (p, Rule (c, fr, t)) ->
     is_within ~low:fr ~high:t ~value:(String.filter p ~f:((=) c) |> String.length)

let is_valid2 = function
  | Password (p, Rule (c, i1, i2)) ->
     let indexMatches ix = String.nget p ix = c in
     let lowIndex = i1 - 1 in
     let highIndex = i2 - 1 in
     indexMatches lowIndex <> indexMatches highIndex

let input = AocLib.lines_of_file_map ~f:parse_input "input/02"

let solve ~f =
  List.filter ~f:f input |> List.length |> Int.to_string

let main =
  AocLib.print_strings ["Part 1: "; (solve ~f:is_valid); "\n";
                        "Part 2: "; (solve ~f:is_valid2); "\n"]
