open OurBase
(** The simples location possible. Identified just by it's name *)

module LocationSetOver (L : ProgramTypes.Location) : sig
  include ProgramTypes.LocationSet with type elt = L.t and type elt_comparator_witness = L.comparator_witness
end

include ProgramTypes.Location

val of_string : string -> t
(** Creates a location from a given name. *)

include Sexpable.S with type t := t
