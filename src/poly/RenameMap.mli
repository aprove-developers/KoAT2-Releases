open OurBase
(** Module provides mapping from variables to other variables. *)

(** A rename map is a function which maps from a finite set of variables to another finite set of variables *)

type t
(** Type of map. *)

type var = Var.t
(** Type of variables. *)

val from : (var * var) list -> t
(** Creates a rename map from a two variable association list *)

val of_sequence : (var * var) Sequence.t -> t
(** Creates a rename map from a two variable association enum *)

val from_native : (string * string) list -> t
(** Creates a rename map from a two strings (vars) association list *)

val id : var list -> t
(** Creates a rename map where every variable keeps its name *)

val find : var -> t -> default:var -> var
(** Returns the new name of the variable or a default value, if the rename map does not assign a new name to the variable *)
