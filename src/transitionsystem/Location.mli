(** The simples location possible. Identified just by it's name *)
open OurBase

module LocationSetOver(L: ProgramTypes.Location) : sig
  include ProgramTypes.LocationSet
    with type elt = L.t
     and type comparator_witness = L.comparator_witness
end

include ProgramTypes.Location

(** Creates a location from a given name. *)
val of_string : string -> t

include Sexpable.S with type t := t
