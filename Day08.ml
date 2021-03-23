open Core

let input = AocLib.lines_of_file_map ~f:(String.split ~on:' ') "input/08"

let decide_steps = function
  | Some (ins :: steps :: _) when ins = "jmp" -> Int.of_string steps
  | _ -> 1

let decide_accumulate = function
  | Some (ins :: value :: _) when ins = "acc" -> Int.of_string value
  | _ -> 0

let run ~program =
  let rec r ~acc ~index ~visited =
    match List.mem ~equal:(=) visited index with
    | true -> acc
    | false ->
       let instruction = List.nth program index in
       let next_index = index + (decide_steps instruction) in
       let next_accumulator = acc + (decide_accumulate instruction) in
       r ~acc:next_accumulator ~index:next_index ~visited:(index :: visited)
  in
  r ~acc:0 ~index:0 ~visited:[]

let main =
  let solution_1 = run ~program:input |> Int.to_string in
  AocLib.print_two_part_solution solution_1 "?"
