open Batteries

(** Provides all module types related to polynomials *)

(** Modules including Eq hold a type that defines an equality relation on its elements *)
module type Eq =
  sig
    type t
    val (==) : t -> t -> bool
    (* TODO val (!=) : t -> t -> bool *)
  end

(** An ID is a unique identifier for the elements of an arbitrary set (of variables) *)
module type ID =
  sig
    type t
    include Eq with type t := t
    val of_string : string -> t
    val to_string : t -> string
    val compare : t -> t -> int
  end

(** Modules including BasePartialOrder fulfil all requirements to become a partial order.
    They can be typeclass-like extended by MakePartialOrder. *)
module type BasePartialOrder =
  sig
    type t
    include Eq with type t := t
    val (>) : t -> t -> bool Option.t
  end

(** Modules including PartialOrder hold a type that defines a partial order on its elements *)
module type PartialOrder =
  sig
    include BasePartialOrder
    val (<) : t -> t -> bool Option.t
    val (>=) : t -> t -> bool Option.t      
    val (>=) : t -> t -> bool Option.t
  end

(** Extends a BasePartialOrder to get all the methods of a partial order *)
module MakePartialOrder(Base : BasePartialOrder) : (PartialOrder with type t := Base.t) =
  struct
    include Base
    let (>=) b1 b2 = Option.map (fun greater -> b1 == b2 || greater) (b1 > b2)
    let (<) b1 b2 = Option.map not (b1 >= b2)
    let (<=) b1 b2 = Option.map not (b1 > b2)
  end

(** A valuation is a function which maps from a finite set of variables to values *)
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

(** This module type defines how functors constructing valuations have to be defined *)
module type ValuationFunctor =
  functor (Var : ID)(Value : Number.Numeric) -> Valuation with type var = Var.t
                                                           and type value = Value.t

(** A rename map is a function which maps from a finite set of variables to another finite set of variables *)
module type RenameMap =
  sig
    type t
    type var
    val from : (var * var) list -> t
    val id : var list -> t
    val find : var -> t -> var -> var
  end

(** Evaluable is a unified interface of all parts of a polynomial *)
module type Evaluable =
  sig
    type t
    module Var : ID
    module Value : Number.Numeric
    module Valuation_ : (Valuation with type var = Var.t and type value = Value.t)
    module RenameMap_ : (RenameMap with type var = Var.t)
    val (==) : t -> t -> bool
    val of_string : string -> t
    val to_string : t -> string
    val vars : t -> Var.t Set.t
    val eval : t -> Valuation_.t -> Value.t
    val rename : RenameMap_.t -> t -> t
    val degree : t -> int
  end

(** This module type defines how functors constructing evaluables have to be defined *)
module type EvaluableFunctor =
  functor (Var : ID)(Value : Number.Numeric) -> Evaluable with module Var = Var
                                                           and module Value = Value

(** A power is a single variable with a positive integer exponent *)
module type Power =
  sig
    type t
    include Evaluable with type t := t
    val make : Var.t -> int -> t
    val lift : Var.t -> t
    val var : t -> Var.t
    val n : t -> int

    val fold : const:(Value.t -> 'b) ->
               var:(Var.t -> 'b) ->
               times:('b -> 'b -> 'b) ->
               pow:('b -> int -> 'b) ->
               t -> 'b 
  end

(** A monomial is a finite product of powers *)
module type Monomial =
  sig
    type t
    include Evaluable with type t := t
    val make : (Var.t * int) list -> t
    val lift : Var.t -> int -> t
    val degree_variable : Var.t -> t -> int
    val delete_var : Var.t -> t -> t
    val simplify : t -> t
    val is_univariate_linear : t -> bool
    val mul : t -> t -> t
    val one : t

    val fold : const:(Value.t -> 'b) ->
               var:(Var.t -> 'b) ->
               times:('b -> 'b -> 'b) ->
               pow:('b -> int -> 'b) ->
               t -> 'b 
  end

(** A scaled monomial is a monomial multiplied with a coefficient *)
module type ScaledMonomial =
  sig
    type t
    type monomial
    include Evaluable with type t := t
    include PartialOrder with type t := t

    val make : Value.t -> monomial -> t
    val lift : monomial -> t
    val simplify : t -> t
    val mul : t -> t -> t
    val mult_with_const : Value.t -> t -> t
    val one : t
    val coeff : t -> Value.t
    val monomial : t -> monomial

    val fold : const:(Value.t -> 'b) ->
               var:(Var.t -> 'b) ->
               times:('b -> 'b -> 'b) ->
               pow:('b -> int -> 'b) ->
               t -> 'b 
  end

(** Modules including BaseMath define basic math operations
    They can be typeclass-like extended by MakeMath *)
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

(** Modules including Math hold a type that defines basic math operations on its elements *)
module type Math =
  sig
    include BaseMath
    val sum : t list -> t
    val product : t list -> t
    val sub : t -> t -> t
  end

(** Extends a BaseMath module to get all math methods *)
module MakeMath(Base : BaseMath) : (Math with type t := Base.t) =
  struct
    include Base
    let sum = List.fold_left add zero
    let product = List.fold_left mul one
    let sub t1 t2 = add t1 (neg t2)
  end

(** A Polynomial represents a mathematical polynomial *)
module type Polynomial =
  sig
    type t
    type monomial

    include Evaluable with type t := t
    include Math with type t := t
    include PartialOrder with type t := t
          
    (* Creation *)
    val make : (Value.t * monomial) list -> t
    val lift : Value.t -> monomial -> t
    val from_var : Var.t -> t
    val from_constant : Value.t -> t
    val from_var_string : string -> t
    val from_constant_int : int -> t
    val from_power : Var.t -> int -> t
    val from_monomial : monomial -> t

    val fold : const:(Value.t -> 'b) ->
               var:(Var.t -> 'b) ->
               neg:('b -> 'b) ->               
               plus:('b -> 'b -> 'b) ->
               times:('b -> 'b -> 'b) ->
               pow:('b -> int -> 'b) ->
               t -> 'b 

    (* Get data *)
    val coeff : monomial -> t -> Value.t
    val monomials : t -> monomial list
    val constant : t -> Value.t
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
    val replace : t -> Valuation_.t -> t
    val delete_monomial : monomial -> t -> t
    val simplify : t -> t
    val mult_with_const : Value.t -> t -> t
  end

(** A MinMaxPolynomial is a polynomial which allows the usage of min and max functions  *)
module type MinMaxPolynomial =
  sig
    type t
    include Evaluable with type t := t
    include BaseMath with type t := t
    include PartialOrder with type t := t
    module Polynomial_ : (Polynomial with module Var = Var and module Value = Value)
    val of_poly : Polynomial_.t -> t              
    val of_constant : Value.t -> t
    val min : t -> t -> t
    val max : t -> t -> t
    val minimum : t list -> t
    val maximum : t list -> t

    val fold : const:(Value.t -> 'b) ->
               var:(Var.t -> 'b) ->
               neg:('b -> 'b) ->               
               plus:('b -> 'b -> 'b) ->
               times:('b -> 'b -> 'b) ->
               pow:('b -> 'b -> 'b) ->
               min:('b -> 'b -> 'b) -> 
               max:('b -> 'b -> 'b) ->
               inf:'b ->
               t -> 'b 
  end
