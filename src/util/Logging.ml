(** Implemenation of different loggers. *)
open Batteries

type logger =
  | Approximation (**  Logger handling approximations *)
  | Size (** Logger handling size-bounds *)
  | Time (** Logger handling time-bounds *)
  | PRF (** Logger handling (multiphase) ranking function creation *)
  | Bound (**  Logger handling  simplification of bounds *)
  | LocalSizeBound (**  Logger handling local size-bounds  *)
  | Preprocessor (**  Logger handling preprocessors  *)
  | CFR (**  Logger handling control flow refinement  *)

(** List of all available loggers *)
let loggers = [Approximation; Size; Time; PRF; Bound; LocalSizeBound; Preprocessor; CFR]

(** List of all available loggers *)
let all = [Approximation; Size; Time; PRF; Bound; LocalSizeBound; Preprocessor; CFR]

(** Returns a string matching to the given logger. *)          
let show_logger = function
  | Approximation -> "appr"
  | Size -> "size"
  | Time -> "time"
  | PRF -> "prf"
  | Bound -> "bound"
  | LocalSizeBound -> "lsb"
  | Preprocessor -> "preprocessor"
  | CFR -> "cfr"

let get =
  Logger.make_log % show_logger

let with_disabled_loggers (logs: (logger * Logger.level) list) =
  loggers
  |> List.map (fun logger ->
         show_logger logger, List.find_opt (fun (l, level) -> l == logger) logs |> Option.map Tuple2.second |? Logger.NONE
       )
  
let use_loggers (logs: (logger * Logger.level) list) =
  Logger.init
    (with_disabled_loggers logs)
    (Logger.make_dbg_formatter IO.stdout)

