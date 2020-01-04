(** MaybeChanged monad represents a value that might change during a computation. *)
open Batteries

(** A type that is wrapped by the MaybeChanged monad represents a value that might change during a computation.
    Once it has changed it will be forever marked as changed, but can also be further be manipulated. *)

(** Status of an object. *)
type status = Changed | Same

(** Type of an object. *)
type 'a t = status * 'a

(** Returns the monad {i ([Same], o)} for an object {i o}. *)
let return subject = (Same, subject)

(** Applies function {i f} on the monad {i ([status], o)} and changes status iff status is [Same] to status of {i f(o)}. *)
let (>>=) maybe f = match maybe with
  | (Changed, subject) -> (Changed, let (_, s) = f subject in s)
  | (Same, subject) -> f subject

(** Applies function {i f} on the monad {i ([status], o)} and changes status iff status is [Same] to status of {i f(o)}. *)
let flat_map f maybe = maybe >>= f

(** Applies function {i f} on the monad {i ([status], o)} and returns the resulting monad {i ([status], f(o))}. *)         
let map f = function
  | (status, subject) -> (status, f subject)

(** Returns the object itself without the monad structure. *)
let unpack = Tuple2.second

(**  Returns true iff. the object has changed. *)
let has_changed = function
  | (Changed, _) -> true
  | _ -> false

(** Applies function {i f} on the monad {i ([status], o)} iff [status] is [Changed] and returns the resulting monad {i ([Changed], f(o))} or the original iff [status] equals [Same]. *)         
let if_changed f = function
  | (Changed, subject) -> (Changed, f subject)
  | (Same, subject) -> (Same, subject)

(** Returns the monad {i ([Changed], o)} for an object {i o}. *)  
let changed subject = (Changed, subject)

(** Returns the monad {i ([Same], o)} for an object {i o}. *)
let same subject = (Same, subject)

(** TODO doc*)
let fold_enum (f: 'a -> 'b -> 'a t) (subject: 'a) (enum: 'b Enum.t) : 'a t =
  Enum.fold (fun maybe_changed element -> maybe_changed >>= fun subject -> f subject element) (same subject) enum
