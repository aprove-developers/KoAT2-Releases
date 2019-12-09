open Batteries
open BoundsInst
open ProgramTypes
open RVGTypes

type kind = [ `Lower | `Upper ] [@@deriving eq, ord, show]

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

    val get : kind -> t -> Trans.t -> Var.t -> B.t

    val add : kind -> B.t -> Trans.t -> Var.t -> t -> t

    val add_all : kind -> B.t -> RV.t list -> t -> t

    val add_all_abs : B.t -> RV.t list -> t -> t

    val to_string : ?print_lower:bool -> t -> string

    val equivalent : t -> t -> bool
  end
