(** Handles shell arguments and computes ranking functions for a program. *)
open Koat2
open Koat2.OurBase
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
  Stdio.printf "prog %s\n\n" (Program.to_string_pretty prog);
  let plrfs =
    Set.to_list gts
    |> List.map ~f:(Plrf.find prog)
    |> List.filter_opt
  in
  Stdio.printf "\n";
  List.iter ~f:(Stdio.printf "%s\n" % Plrf.to_string) plrfs
