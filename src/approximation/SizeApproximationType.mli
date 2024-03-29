open! OurBase

type ('rvtuple_, 'bound, 'rvtuple__cmp_wit) size_approximation_t

module Make (B : BoundType.Bound) (RV : ProgramTypes.RV) : sig
  type t = (RV.t, B.t, RV.comparator_witness) size_approximation_t

  val empty : t
  val get : t -> RV.t -> B.t
  val filter_rvs : (RV.t -> bool) -> t -> t
  val add : ?simplifyfunc:(B.t -> B.t) -> B.t -> RV.t -> t -> t
  val add_all : ?simplifyfunc:(B.t -> B.t) -> B.t -> RV.t list -> t -> t
  val to_formatted : ?pretty:bool -> t -> FormattedString.t
  val to_string : t -> string
  val to_sequence : t -> (RV.t * B.t) Sequence.t
  val of_sequence : (RV.t * B.t) Sequence.t -> t
end
