open Batteries

type logger =
  | Approximation
  | Bound
  | BoundWrapper
  | BottomUp
  | ELSB
  | ExactRuntime
  | ExpSize
  | ExpTime
  | LexRSM
  | LocalSizeBound
  | MeteringRSM
  | PRF
  | Preprocessor
  | Size
  | Time

let loggers =
  [
    Approximation;
    BottomUp;
    Bound;
    BoundWrapper;
    ELSB;
    ExactRuntime;
    ExpSize;
    ExpTime;
    LexRSM;
    LocalSizeBound;
    MeteringRSM;
    PRF;
    Preprocessor;
    Size;
    Time;
  ]

let all = loggers

let show_logger = function
  | Approximation  -> "appr"
  | Bound          -> "bound"
  | BoundWrapper   -> "boundWrapper"
  | BottomUp       -> "bottomUp"
  | ELSB           -> "elcb"
  | ExactRuntime   -> "exactruntime"
  | ExpSize        -> "expSize"
  | ExpTime        -> "expTime"
  | LexRSM         -> "lexrsmMap"
  | LocalSizeBound -> "lsb"
  | MeteringRSM    -> "metrsmMap"
  | PRF            -> "prf"
  | Preprocessor   -> "preprocessor"
  | Size           -> "size"
  | Time           -> "time"

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

