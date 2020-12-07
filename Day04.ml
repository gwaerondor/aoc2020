open Core

type passport_info = Info of string option
type passport = {byr : passport_info;
                 iyr : passport_info;
                 eyr : passport_info;
                 hgt : passport_info;
                 hcl : passport_info;
                 ecl : passport_info;
                 pid : passport_info;
                 cid : passport_info}

let create_passport = {byr = Info None;
                       iyr = Info None;
                       eyr = Info None;
                       hgt = Info None;
                       hcl = Info None;
                       ecl = Info None;
                       pid = Info None;
                       cid = Info None}

let add_passport_info pp s =
  match String.split ~on:':' s with
  | ["byr"; x] -> {pp with byr = (Info (Some x))}
  | ["iyr"; x] -> {pp with iyr = (Info (Some x))}
  | ["eyr"; x] -> {pp with eyr = (Info (Some x))}
  | ["hgt"; x] -> {pp with hgt = (Info (Some x))}
  | ["hcl"; x] -> {pp with hcl = (Info (Some x))}
  | ["ecl"; x] -> {pp with ecl = (Info (Some x))}
  | ["pid"; x] -> {pp with pid = (Info (Some x))}
  | ["cid"; x] -> {pp with cid = (Info (Some x))}
  | [""] -> pp
  | _ -> AocLib.halt "Unknown passport info string\n"

let parse_passport lines =
  let tokenize = String.split_on_chars ~on:[' '; '\n'] in
  tokenize lines
  |> List.fold_left ~f:add_passport_info ~init:create_passport

let input =
  let splitter = Re2.split (Re2.create_exn "\n\n") in
  AocLib.contents_of_file_map ~f:splitter "input/04"
  |> List.map ~f:parse_passport

let is_valid pp =
  let missing_value = function
    | Info None -> true
    | Info (Some _) -> false
  in
  List.filter ~f:missing_value [pp.byr; pp.iyr; pp.eyr; pp.hgt;
                                pp.hcl; pp.ecl; pp.pid]
  |> List.is_empty

let is_valid2 pp =
  let m ~regex v =
    let re = (Re2.create_exn regex) in
    match v with
    | Info (Some x) -> Re2.matches re x
    | Info None -> false
  in
  is_valid pp
  && m ~regex: "^(19[2-9][0-9]|200[0-2])$" pp.byr
  && m ~regex: "^20(1[0-9]|20)$" pp.iyr
  && m ~regex: "^20(2[0-9]|30)$" pp.eyr
  && m ~regex: "^((1[5-8][0-9]|19[0-3])cm)|((59|6[0-9]|7[0-6])in)$" pp.hgt
  && m ~regex: "^#[0-9a-f]{6}$" pp.hcl
  && m ~regex: "^(amb|blu|brn|gry|grn|hzl|oth)$" pp.ecl
  && m ~regex: "^[0-9]{9}$" pp.pid

let print_passport pp =
  let ts = function | Info (Some x) -> x | _ -> "-" in
  AocLib.print_strings ["[byr: "; ts pp.byr; ", iyr: "; ts pp.iyr;
                        ", eyr: "; ts pp.eyr; ", hgt: "; ts pp.hgt;
                        ", hcl: "; ts pp.hcl; ", ecl: "; ts pp.ecl;
                        ", pid: "; ts pp.pid; ", cid: "; ts pp.cid; "]\n"]

let main =
  let solution_1 = List.filter ~f:is_valid input |> AocLib.list_length_str in
  let solution_2 = List.filter ~f:is_valid2 input |> AocLib.list_length_str in
  AocLib.print_two_part_solution solution_1 solution_2
