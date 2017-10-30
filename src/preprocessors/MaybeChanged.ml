open Batteries

(** A type that is wrapped by the MaybeChanged monad represents a value that might change during a computation.
    Once it has changed it will be forever marked as changed, but can also be further be manipulated. *)

type status = Changed | Same

type 'a t = status * 'a

let return subject = (Same, subject)

let (>>=) maybe f = match maybe with
  | (Changed, subject) -> (Changed, let (_, s) = f subject in s)
  | (Same, subject) -> f subject

let map f = function
  | (status, subject) -> (status, f subject)

let unpack = Tuple2.second

let has_changed = function
  | (Changed, _) -> true
  | _ -> false

let changed subject = (Changed, subject)

let same subject = (Same, subject)

let lift_to_program (transform: Program.TransitionGraph.t -> Program.TransitionGraph.t t) (program: Program.t): Program.t t =
  transform (Program.graph program) >>= (fun graph -> same (Program.map_graph (fun _ -> graph) program))

let lift_to_subject (transform: Program.t -> Program.t t) (subject: 'a): 'a t =
  transform (Tuple2.first subject) >>= (fun program -> same (Tuple2.map1 (fun _ -> program) subject))
