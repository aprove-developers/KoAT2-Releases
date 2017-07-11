open Batteries

module type Evaluable =
  sig
    type t
    type value
    type var
    type valuation
    type rename_map
    val (==) : t -> t -> bool
    val of_string : string -> t
    val to_string : t -> string
    val compare : t -> t -> int
    val vars : t -> var list
    val eval : t -> valuation -> value
    val to_z3 : Z3.context -> t -> Z3.Expr.expr
    val rename : rename_map -> t -> t
    val degree : t -> int
  end

module type ID =
  sig
    type t
    val (==) : t -> t -> bool
    val of_string : string -> t
    val to_string : t -> string
    val compare : t -> t -> int
  end
  
module type EvaluableFunctor =
  functor (Var : ID) -> Evaluable with type var = Var.t

module type Power =
  sig
    type t
    include Evaluable with type t := t
    val make : var -> int -> t
    val lift : var -> t
    val var : t -> var
    val n : t -> int
  end
                      
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
  
module type Valuation =
  sig
    type t
    type var
    type value = Big_int.big_int
    val from : (var * value) list -> t
    val zero : var list -> t
    val eval : var -> t -> value
    val vars : t -> var list
  end

module type ValuationFunctor =
  functor (Id : ID) -> Valuation with type var = Id.t
                                  and type value = Big_int.big_int
