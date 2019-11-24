open Batteries

let read_input trans_id_counter ?(rename=false) simple program_str =
  if simple then
    program_str
    |> Readers.read_program_simple trans_id_counter
    |> (if rename then Program.rename else identity)
    |> Option.some
  else
    try
      program_str
      |> Readers.read_file trans_id_counter
      |> (if rename then Program.rename else identity)
      |> Option.some
    with TransitionLabel.RecursionNotSupported ->
      prerr_string "ERROR: The given program uses recursion. Recursion is not supported by the current version of koat2. The program will exit now."; None

let read_input_goal trans_id_counter ?(rename=false) simple program_str =
  if simple then
      program_str
      |> Readers.read_program_simple trans_id_counter
      |> (if rename then Program.rename else identity)
      |> fun program -> Some (program, "UNKNOWN")
  else
    try
      program_str
      |> Readers.read_prog_goal_file trans_id_counter
      |> (if rename then Tuple2.map1 (Program.rename) else identity)
      |> Option.some
      with TransitionLabel.RecursionNotSupported ->
        prerr_string "ERROR: The given program uses recursion. Recursion is not supported by the current version of koat2. The program will exit now.";
        None

let read_goal simple program_str =
  if simple then "COMPLEXITY"
  else
  program_str
  |> Readers.read_goal_file

