open Batteries
open BoundsInst
open ProgramModules
open RVGTypes

module Make_SizeApproximation :
  functor (Num : PolyTypes.OurNumber)
          (Poly :
             sig
               include PolyTypes.Polynomial with type value = Num.t
                                             and type valuation = Valuation.Make(Num).t
                                             and type monomial = Monomials.Make(Num).t
               val max_of_occurring_constants : t -> Num.t
             end )
          (Trans :
             sig
               type t
               val same: t -> t -> bool
               val id: t -> int
               val src: t -> Location.t
               val target_string: t -> string
               val to_id_string: t -> string
               val compare_same: t -> t -> int
             end)
          (RV :
             sig
               type t = Trans.t * Var.t
               val to_id_string: t -> string
             end) ->
  sig
    module B : sig include module type of BoundType.Make_BoundOver(Num)(Poly) end

    type t

    val empty : int -> t

    val get : t -> Trans.t -> Var.t -> B.t

    val add : ?simplifyfunc:(B.t -> B.t) -> B.t -> Trans.t -> Var.t -> t -> t

    val add_all : ?simplifyfunc:(B.t -> B.t) -> B.t -> RV.t list -> t -> t

    val to_formatted : ?pretty:bool -> t -> FormattedString.t

    val to_string : t -> string

    val equivalent : t -> t -> bool
  end
