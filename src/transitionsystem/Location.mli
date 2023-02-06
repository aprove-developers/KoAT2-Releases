(** The simples location possible. Identified just by it's name *)

module LocationSetOver(L: ProgramTypes.Location) : sig
  include ProgramTypes.LocationSet
    with type elt = L.t
end

include ProgramTypes.Location

(** Creates a location from a given name. *)
val of_string : string -> t

