open Batteries
open ProgramTypes

let description = "Search for linear probabilistic ranking function"

let command = "pprf"

type params = {
    input : string; [@aka ["i"]] [@pos 0]
    (** Either an absolute or relative path to the koat input file which defines the integer transition system.
        Or the program defined in simple mode.
        How this string is interpreted is defined by the simple-input flag *)

    simple_input : bool; [@default false] [@aka ["s"]]
    (** If the simple-input flag is set, the input is not interpreted as a filepath, but as a program in simple mode. *)

  } [@@deriving cmdliner, show]

let run (params: params) =
  let cache = CacheManager.new_cache () in

  Logging.(use_loggers [LexRSM, Logger.DEBUG; Preprocessor, Logger.DEBUG]);
  params.input
  |> MainUtil.read_input_goal (CacheManager.trans_id_counter cache) false
  |> Option.may (fun (program, _) ->
        let gts =
          (program, Approximation.create program)
          |> Preprocessor.process (CacheManager.trans_id_counter cache) Preprocessor.process_til_fixpoint Preprocessor.([InvariantGeneration; ProbabilityLessOne])
          (* get program *)
          |> Tuple2.first
          |> Program.generalized_transitions
          |> GeneralTransitionSet.to_list
        in
        gts
        |> List.filter (not % Program.is_initial_gt program)
        |> List.map (LexRSM.find (CacheManager.lrsm_cache cache) program)
        |> List.filter (Option.is_some)
        |> Util.option_sequence
        |> Option.may (fun pprflist ->
             List.map LexRSM.pprf_to_string pprflist
             |> String.concat "\n"
             |> flip (^) "\n"
             |> print_string) )
