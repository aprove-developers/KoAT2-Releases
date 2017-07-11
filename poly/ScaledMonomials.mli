open Batteries
open ID
open Evaluable

module type ScaledMonomial =
  sig
    type t
    type power
    type monomial
    include Evaluable with type t := t
    val make : value -> monomial -> t
    val lift : monomial -> t
    val simplify : t -> t
    val mult : t -> t -> t
    val mult_with_const : value -> t -> t
    val one : t
    val coeff : t -> value
    val monomial : t -> monomial
  end

module MakeScaledMonomial(Var : ID) : ScaledMonomial with type var = Var.t
                                                      and type rename_map = Var.t Map.Make(Var).t
                                                      and type value = Big_int.big_int
                                                      and type valuation = Valuation.MakeValuation(Var).t
                                                      and type power = Powers.MakePower(Var).t
                                                      and type monomial = Monomials.MakeMonomial(Var).t


module StringScaledMonomial : ScaledMonomial

