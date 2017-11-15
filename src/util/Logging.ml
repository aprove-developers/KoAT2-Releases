open Batteries

type logger =
  | Approximation
  | Size
  | Time
  | PRF
  | Bound
  | LocalSizeBound

let loggers = [Approximation; Size; Time; PRF; Bound; LocalSizeBound]

let show_logger = function
  | Approximation -> "appr"
  | Size -> "size"
  | Time -> "time"
  | PRF -> "prf"
  | Bound -> "bound"
  | LocalSizeBound -> "lsb"

let get =
  Logger.make_log % show_logger
   
let use_loggers (logs: (logger * Logger.level) list) =
  Logger.init
    (List.map (Tuple2.map1 show_logger) logs)
    (Logger.make_dbg_formatter IO.stdout)

