open Batteries
open ID
open Evaluable

module type Monomial =
  sig
    type t
    type power
    include Evaluable with type t := t
    val make : power list -> t
    val lift : power -> t
    val degree_variable : var -> t -> int
    val delete_var : var -> t -> power list
    val simplify : t -> t
    val is_univariate_linear : t -> bool
    val mult : t -> t -> t
    val one : t
  end

module MakeMonomial(Var : ID) : Monomial with type var = Var.t
                                          and type rename_map = Var.t Map.Make(Var).t
                                          and type value = Big_int.big_int
                                          and type valuation = Valuation.MakeValuation(Var).t
                                          and type power = Powers.MakePower(Var).t


module StringMonomial : Monomial

