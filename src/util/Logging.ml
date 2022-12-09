(** Implemenation of different loggers. *)
open Batteries

type logger =
  | Approximation (**  Logger handling approximations *)
  | Bound (**  Logger handling  simplification of bounds *)
  | CFR (**  Logger handling control flow refinement  *)
  | ExpTime (** Logger handling computation of expected time bounds *)
  | Inv (** Logger handling invariant creation *)
  | LocalSizeBound (**  Logger handling local size-bounds  *)
  | PLRF (** Logger handling probabilistic linear ranking functions *)
  | PRF (** Logger handling (multiphase) ranking function creation *)
  | Preprocessor (**  Logger handling preprocessors  *)
  | Program (** Logger handling creation of programs, e.g., possible elimination of recursion *)
  | Size (** Logger handling size-bounds *)
  | Time (** Logger handling time-bounds *)
  | Twn (** Logger handling twn local time-bound computation *)

(** List of all available loggers *)
let loggers = [Approximation; Size; Time; PRF; Bound; LocalSizeBound; Program; Preprocessor; CFR; Inv; Twn]

(** List of all available loggers *)
let all = [Approximation; Size; Time; PRF; Bound; LocalSizeBound; Program; Preprocessor; CFR; Inv; Twn]

(** Returns a string matching to the given logger. *)
let show_logger = function
  | Approximation -> "appr"
  | Bound -> "bound"
  | CFR -> "cfr"
  | ExpTime -> "exptime"
  | Inv -> "invariants"
  | LocalSizeBound -> "lsb"
  | PLRF -> "plrf"
  | PRF -> "prf"
  | Preprocessor -> "preprocessor"
  | Program -> "program"
  | Size -> "size"
  | Time -> "time"
  | Twn -> "twn"

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

