(** Implemenation of a preprocessor which normalises updates. *)
open Batteries
open ProgramTypes

(** This preprocessor fills up all transitions Arg_1,...,Arg_i -> u(Arg_1),...,u(Arg_j) to Arg_1,...,Arg_n -> u(Arg_1),...,u(Arg_n)*)

(** Logger Preprocessor *)
let logger = Logging.(get Preprocessor)

let normalise program =
    let input_vars = (Program.input_vars program) in
    let trans = Program.transitions program
    |> TransitionSet.map (fun (l,g,l') -> (l,TransitionLabel.normalise g input_vars,l'))
    |> TransitionSet.to_list in
    Program.from trans (Program.start program)