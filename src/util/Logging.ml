open Batteries

type logger =
  | Approximation
  | Size
  | ExpTrivialSize
  | ExpNontrivialSize
  | Time
  | PRF
  | Bound
  | LocalSizeBound
  | Preprocessor
  | LexRSM
  | MeteringRSM

let loggers = [Approximation; Size; ExpTrivialSize; ExpNontrivialSize; Time; PRF; Bound; LocalSizeBound; Preprocessor; LexRSM; MeteringRSM]

let all = [Approximation; Size; ExpTrivialSize; ExpNontrivialSize; Time; PRF; Bound; LocalSizeBound; Preprocessor; LexRSM; MeteringRSM]
            
let show_logger = function
  | Approximation -> "appr"
  | Size -> "size"
  | ExpTrivialSize -> "ExpTrivialSize"
  | ExpNontrivialSize -> "ExpNontrivialSize"
  | Time -> "time"
  | PRF -> "prf"
  | Bound -> "bound"
  | LocalSizeBound -> "lsb"
  | Preprocessor -> "preprocessor"
  | LexRSM -> "lexrsmMap"
  | MeteringRSM -> "metrsmMap"

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

