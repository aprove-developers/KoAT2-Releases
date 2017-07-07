type t = {
  name : string;
  tail : Locations.t;
  head : Locations.t;
  cost : Big_int.big_int;
}
val equal : t -> t -> bool
val compare : t -> t -> int
val default : t
