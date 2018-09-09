open Batteries

module type OurNumber = 
  sig
    include Number.Numeric

    val (=~=) : t -> t -> bool
    val pow : t -> int -> t
    val max : t -> t -> t
    val min : t -> t -> t
  end
