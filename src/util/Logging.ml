open Batteries

type logger =
  | Approximation
  | Size
  | Time
  | PRF
  | Bound
  | LocalSizeBound
  | Preprocessor
  | LexRSM
  | MeteringRSM
  | Roots

let loggers = [Approximation; Size; Time; PRF; Bound; LocalSizeBound; Preprocessor; LexRSM; MeteringRSM; Roots]

let all = [Approximation; Size; Time; PRF; Bound; LocalSizeBound; Preprocessor; LexRSM; MeteringRSM; Roots]
            
let show_logger = function
  | Approximation -> "appr"
  | Size -> "size"
  | Time -> "time"
  | PRF -> "prf"
  | Bound -> "bound"
  | LocalSizeBound -> "lsb"
  | Preprocessor -> "preprocessor"
  | LexRSM -> "lexrsmMap"
  | MeteringRSM -> "metrsmMap"
  | Roots -> "roots"

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

