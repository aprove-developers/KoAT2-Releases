open Batteries

let read_process_lines command = (* This function was written by Tom Küspert *)
  let lines = ref [] in
  let in_channel = Unix.open_process_in command in
  begin
    try
      while true do
        lines := input_line in_channel :: !lines
      done;
    with
      | BatInnerIO.Input_closed -> ()
      | End_of_file -> ()
  end;
  List.rev !lines

(* 1) Get transition and extract solvable blocks
   2) Test if RB and Entry Size-Bounds finite
   3) For all blocks with Z[] and a non-polynomial size bound:
   4) Update -> Matrix
   5) Run Python & Update script s.t. we can hand in vars
   6) Sub n -> RB
   8) Add sizebound to appr *)

let run_python =
  let command = "python3 -c 'from src.bounds.SizeBoundSolvable import size_bound; size_bound([2,0,0,3])'" in
  let python_output = read_process_lines command in
  ignore(  match python_output with
      | [a] -> Printf.printf "OUT: \n %s \n" a; Some(a)
      | _ -> Printf.printf "BLUB \n "; None (*error string *) );
  ()
