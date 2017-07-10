open Batteries
open ID
open Evaluable

module type Polynomial =
  sig
    type t
    type power
    type monomial
    type scaled_monomial
    include Evaluable with type t := t
    val make : scaled_monomial list -> t
    val lift : scaled_monomial -> t
    val coeff : monomial -> t -> value
    val delete_monomial : monomial -> t -> t
    val simplify : t -> t
    val to_string : t -> string
    val monomials : t -> monomial list
    val from_var : var -> t
    val zero : t
    val one : t
    val constant : t -> value
    val from_constant : value -> t
    val is_var : t -> bool
    val is_var_plus_constant : t -> bool
    val is_sum_of_vars_plus_constant : t -> bool
    val is_univariate_linear : t -> bool
    val is_const : t -> bool
    val is_linear : t -> bool
    val mult_with_const : value -> t -> t
    val negate : t -> t
    val add : t -> t -> t
    val sum : t list -> t
    val subtract : t -> t -> t
    val mult : t -> t -> t
    val pow : t -> int -> t
  end

module MakePolynomial(Var : ID) : Polynomial with type var = Var.t
                                              and type rename_map = Var.t Map.Make(Var).t
                                              and type value = Big_int.big_int
                                              and type valuation = Valuation.MakeValuation(Var).t
                                              and type power = Powers.MakePower(Var).t
                                              and type monomial = Monomials.MakeMonomial(Var).t
                                              and type scaled_monomial = ScaledMonomials.MakeScaledMonomial(Var).t


module StringPolynomial : Polynomial
