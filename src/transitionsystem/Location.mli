(** The simples location possible. Identified just by it's name *)

include ProgramTypes.Location

(** Creates a location from a given name. *)
val of_string : string -> t
