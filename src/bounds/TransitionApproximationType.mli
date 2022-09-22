open Batteries
open BoundsInst
open Program
(** Abstracts TransitionApproximation so that it can be used to handle normal transitions with integer bounds and general
 * transitions with real bounds*)
module Make_TransitionApproximation :
 functor (Num : PolyTypes.OurNumber) -> functor
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
              val id: t -> int
              val to_id_string: t -> string
              val compare_same: t -> t -> int
              val fold_transset: (t -> 'a -> 'a) -> TransitionSet.t -> 'a -> 'a
            end) ->
   sig
     module B : sig include module type of BoundType.Make_BoundOver(Num)(Poly) end

     type t

     val empty : string -> int -> t

     val get : t -> Trans.t -> B.t

     val get_id : t -> int -> B.t

     (** Returns a timebound of the specified kind for the execution of the whole graph. *)
     val sum : t -> Program.t -> B.t

     val add : ?simplifyfunc:(B.t -> B.t) -> B.t -> Trans.t -> t -> t

     val all_bounded : t -> Trans.t list -> bool

     val to_formatted : ?pretty:bool -> Trans.t list -> t -> FormattedString.t

     val to_string : Trans.t list -> t -> string

     val equivalent : t -> t -> bool
   end
