open Batteries

let description = "Run a size bound improvement step"

let command = "size"

type params = {

    program : string; [@pos 0] [@docv "FILE"]
    (** The file of the program which should be analyzed. *)

  } [@@deriving cmdliner, show]

let run (params: params) =
  Logging.(use_loggers [Size, Logger.DEBUG]);
  let cache = CacheManager.new_cache () in
  let appr = Approximation.empty 10 3 10
  and program = Readers.read_file (CacheManager.trans_id_counter cache) params.program in
  SizeBounds.improve (CacheManager.lsb_cache cache) program appr
  |> Approximation.to_string program false
  |> print_string

