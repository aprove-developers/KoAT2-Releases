open Batteries

module type Eq =
  sig
    type t
    val (==) : t -> t -> bool
    (* TODO val (!=) : t -> t -> bool *)
  end
  
module type ID =
  sig
    type t
    include Eq with type t := t
    val of_string : string -> t
    val to_string : t -> string
    val compare : t -> t -> int
  end

module type BasePartialOrder =
  sig
    type t
    include Eq with type t := t
    val (>) : t -> t -> bool Option.t
  end

module type PartialOrder =
  sig
    include BasePartialOrder
    val (<) : t -> t -> bool Option.t
    val (>=) : t -> t -> bool Option.t      
    val (>=) : t -> t -> bool Option.t
  end

module MakePartialOrder(Base : BasePartialOrder) : (PartialOrder with type t := Base.t) =
  struct
    include Base
    let (>=) b1 b2 = Option.map (fun greater -> b1 == b2 || greater) (b1 > b2)
    let (<) b1 b2 = Option.map not (b1 >= b2)
    let (<=) b1 b2 = Option.map not (b1 > b2)
  end
  
module PolynomialAST(Var : ID) =
  struct
    type t =
      | Constant of int
      | Variable of Var.t
      | Neg of t
      | Plus of t * t
      | Times of t * t
      | Pow of Var.t * int
  end

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
    val vars : t -> var list
    val eval : t -> valuation -> value
    val to_z3 : Z3.context -> t -> Z3.Expr.expr
    val rename : rename_map -> t -> t
    val degree : t -> int
  end

module type EvaluableFunctor =
  functor (Var : ID)(Value : Number.Numeric) -> Evaluable with type var = Var.t

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
    val mul : t -> t -> t
    val one : t
  end

module type ScaledMonomial =
  sig
    type t
    type power
    type monomial
    include Evaluable with type t := t
    include PartialOrder with type t := t

    val make : value -> monomial -> t
    val lift : monomial -> t
    val simplify : t -> t
    val mul : t -> t -> t
    val mult_with_const : value -> t -> t
    val one : t
    val coeff : t -> value
    val monomial : t -> monomial
  end

module type BaseMath =
  sig
    type t
    val zero : t
    val one : t
    val neg : t -> t
    val add : t -> t -> t
    val mul : t -> t -> t
    val pow : t -> int -> t
  end

module type Math =
  sig
    include BaseMath
    val sum : t list -> t
    val product : t list -> t
    val sub : t -> t -> t
  end

module MakeMath(Base : BaseMath) : (Math with type t := Base.t) =
  struct
    include Base
    let sum = List.fold_left add zero
    let product = List.fold_left mul one
    let sub t1 t2 = add t1 (neg t2)
  end

  
module type Polynomial =
  sig
    type t
    type power
    type monomial
    type scaled_monomial
    type polynomial_ast
    type poly_valuation
    
    include Evaluable with type t := t
    include Math with type t := t
    include PartialOrder with type t := t
          
    (* Creation *)
    val make : scaled_monomial list -> t
    val lift : scaled_monomial -> t
    val from_ast : polynomial_ast -> t
    val from_var : var -> t
    val from_constant : value -> t
    val from_power : power -> t
    val from_monomial : monomial -> t
    val from_scaled_monomial : scaled_monomial -> t
      
    (* Get data *)
    val coeff : monomial -> t -> value
    val monomials : t -> monomial list
    val constant : t -> value
    val to_string : t -> string
      
    (* Find out properties *)
    val is_var : t -> bool
    val is_var_plus_constant : t -> bool
    val is_sum_of_vars_plus_constant : t -> bool
    val is_univariate_linear : t -> bool
    val is_const : t -> bool
    val is_linear : t -> bool
    val is_zero : t -> bool
    val is_one : t -> bool

    (* Misc *)
    val replace : t -> poly_valuation -> t
    val delete_monomial : monomial -> t -> t
    val simplify : t -> t
    val mult_with_const : value -> t -> t
  end

module type MinMaxPolynomial =
  sig
    type t
    type polynomial
    type polynomial_ast
    include Evaluable with type t := t
    include BaseMath with type t := t
    include PartialOrder with type t := t
    val of_poly : polynomial -> t              
    val of_constant : value -> t
    val min : t -> t -> t
    val max : t -> t -> t
    val minimum : t list -> t
    val maximum : t list -> t
  end
  
module type Valuation =
  sig
    type t
    type var
    type value
    val from : (var * value) list -> t
    val zero : var list -> t
    val eval : var -> t -> value
    val vars : t -> var list
  end

module type ValuationFunctor =
  functor (Id : ID)(Value : Number.Numeric) -> Valuation with type var = Id.t
                                                          and type value = Value.t
