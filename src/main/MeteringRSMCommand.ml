open Batteries
open ProgramTypes

let description = "Testing for meteringRSM functionality"

let command = "metrsm"

type params = {
    input : string; [@aka ["i"]] [@pos 0]
    (** Either an absolute or relative path to the koat input file which defines the probabilistic integer transition system.
        Or the program defined in simple mode.
        How this string is interpreted is defined by the simple-input flag *)

    simple_input : bool; [@default false] [@aka ["s"]]
    (** If the simple-input flag is set, the input is not interpreted as a filepath, but as a program in simple mode. *)

  } [@@deriving cmdliner, show]

let run (params: params) =
  let cache = CacheManager.new_cache () in

  Logging.(use_loggers [MeteringRSM, Logger.DEBUG]);
  params.input
  |> MainUtil.read_input (CacheManager.trans_id_counter cache) params.simple_input
  |> Option.may (fun program ->
        (program, Approximation.create program)
        |> Preprocessor.process (CacheManager.trans_id_counter cache) Preprocessor.process_til_fixpoint Preprocessor.([InvariantGeneration])
        |> (fun (prog, appr) -> MeteringRSM.find (CacheManager.metring_cache cache) prog))