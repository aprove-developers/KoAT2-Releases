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
