open Batteries
open BoundsInst
open ProgramModules
open RVGTypes

module Make(B: BoundType.Bound)(RV: ProgramTypes.RV):
  sig
    type t

    val empty : int -> t

    val get : t -> RV.t -> B.t

    val add : ?simplifyfunc:(B.t -> B.t) -> B.t -> RV.t -> t -> t

    val add_all : ?simplifyfunc:(B.t -> B.t) -> B.t -> RV.t list -> t -> t

    val to_formatted : ?pretty:bool -> t -> FormattedString.t

    val to_string : t -> string

    val enum: t -> (RV.t * B.t) Enum.t
    val of_enum: (RV.t * B.t) Enum.t -> t
  end

module EqMake(B: BoundType.Bound)
             (RV: ProgramTypes.RV)(RV': ProgramTypes.RV)
             (_: functor(F: functor(_: ProgramTypes.RVTuple) -> sig type t end) -> sig
                val proof: (F(RV.RVTuple_).t, F(RV'.RVTuple_).t) Util.TypeEq.t
              end): sig

  val proof: (Make(B)(RV).t, Make(B)(RV').t) Util.TypeEq.t

end
