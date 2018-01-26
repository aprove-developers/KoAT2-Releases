open Batteries

let read_input ?(rename=false) simple program_str =
  if simple then
    Some (Readers.read_program_simple program_str)
  else
    try
      program_str
      |> Readers.read_file
      |> Option.some
    with TransitionLabel.RecursionNotSupported ->
      prerr_string "ERROR: The given program uses recursion. Recursion is not supported by the current version of koat. The program will exit now."; None

