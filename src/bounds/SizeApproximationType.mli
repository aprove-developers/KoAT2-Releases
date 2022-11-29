open Batteries
open BoundsInst
open ProgramModules
open RVGTypes

module Make
  (Num: PolyTypes.OurNumber)
  (RV: RVGTypes.RVType):
  sig
    module B : sig include module type of BoundType.Make_BoundOver(Num) end

    type t

    val empty : int -> t

    val get : t -> RV.t -> B.t

    val add : ?simplifyfunc:(B.t -> B.t) -> B.t -> RV.t -> t -> t

    val add_all : ?simplifyfunc:(B.t -> B.t) -> B.t -> RV.t list -> t -> t

    val to_formatted : ?pretty:bool -> t -> FormattedString.t

    val to_string : t -> string

    val equivalent : t -> t -> bool
  end
