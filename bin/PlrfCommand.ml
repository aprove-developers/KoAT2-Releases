(** Handles shell arguments and computes ranking functions for a program. *)
open Batteries
open Koat2
open Readers
open ProbabilisticProgramModules

let description = "Search for a probabilistic ranking function"

let command = "plrf"

type params = {
  input : string; [@pos 0]
  (** Path to the koat input file which defines the probabilistic integer transition system. *)
} [@@deriving cmdliner, show]

let run (params: params) =
  Logging.(use_loggers [PRF, Logger.DEBUG; Preprocessor, Logger.DEBUG]);
  let (prog,_) =
    Readers.read_probabilistic_prog_goal_file params.input
  in

  let prog =
    Preprocessor.process_till_fixpoint
      (module NonProbOverappr)
      [ Preprocessor.InvariantGeneration; Preprocessor.CutUnsatisfiableTransitions ]
      prog
  in

  let gts = Program.gts prog in
  Printf.printf "prog %s\n\n" (Program.to_string_pretty prog);
  let plrfs =
    GeneralTransitionSet.to_list gts
    |> List.map (Plrf.find prog)
    |> List.map Option.get % List.filter Option.is_some
  in
  Printf.printf "\n";
  List.iter (Printf.printf "%s\n" % Plrf.to_string) plrfs
