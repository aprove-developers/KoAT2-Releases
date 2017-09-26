open Batteries

(** Provides all module types related to polynomials *)

(** Modules including Eq hold a type that defines a semantic equality relation on its elements.
    (==) is not used for that, because it has a reserved meaning in Ocaml. *)
module type Eq =
  sig
    type t
    val (=~=) : t -> t -> bool
    (* TODO val (!=) : t -> t -> bool *)
  end

(** TODO *)
module type Ring =
  sig
    type t [@@deriving eq]

    val zero : t
    val one : t
    val neg : t -> t
    val add : t -> t -> t
    val mul : t -> t -> t
    val pow : t -> int -> t

    val (=~=) : t -> t -> bool

    val of_int : int -> t
    val to_int : t -> int

    val to_string : t -> string
                          (* val eval : t -> (Var.t -> value) -> value *)
  end

module OurInt : Ring =
  struct
    include Number.MakeNumeric(Big_int)
    let (=~=) = equal
    let pow i (n: int) = pow i (of_int n)
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
    type var = Var.t
    type value

    val from : (var * value) list -> t

    (** Creates a valuation from a string (var) to int (value) association list *)
    val from_native : (string * int) list -> t

    (** Creates a valuation where every variable is assigned the value zero *)
    val zero : var list -> t

    (** Returns the value of the variable.
        !! If the valuation does not provide a value for a variable, an exception is raised. !! *)
    val eval : var -> t -> value

    (** Returns the value of the variable, if the valuation defines one for it. *)
    val eval_opt : var -> t -> value Option.t

    (** Returns a list of the variables for which the valuation is defined *)
    val vars : t -> var list

    (** Returns the var to value bindings. *)
    val bindings : t -> (var * value) Enum.t
  end

(** Evaluable is a unified interface of all parts of a polynomial *)
module type Evaluable =
  sig
    type t
    module Value : Ring
    module Valuation_ : (Valuation with type var = Var.t and type value = Value.t)

    include Eq with type t := t
    val to_string : t -> string

    (** Returns a set of the variables which occur in the evaluable *)
    val vars : t -> Var.t Set.t

    (** Assigns each variable inside the polynomial the value of the valuation and returns the arithmetically computed result. 
        !! If the valuation does not provide a value for a variable, an exception is raised. !! *)
    val eval : t -> Valuation_.t -> Value.t

    val eval_f : t -> (Var.t -> Value.t) -> Value.t

    (** Assigns the variables of the evaluable new names based on the rename map. *)
    val rename : RenameMap.t -> t -> t

    val degree : t -> int
  end

(** This module type defines how functors constructing evaluables have to be defined *)
module type EvaluableFunctor =
  functor (Value : Ring) -> Evaluable with module Value = Value

(** A monomial is a finite product of powers *)
module type Monomial =
  sig
    type t
    include Evaluable with type t := t

    val make : (Var.t * int) list -> t
    val lift : Var.t -> int -> t

    (** Returns the degree of the given variable. *)
    val degree_variable : Var.t -> t -> int

    (** Deletes all occurences of the given variable. *)
    val delete_var : Var.t -> t -> t

    (** Returns if the monomial is of the simplified form x^1 for any variable x. *)
    val is_univariate_linear : t -> bool

    (** Multiplies two monomials. The result is always a monomial. *)
    val mul : t -> t -> t

    (** Returns a monomial representing the numeric value one (1). *)
    val one : t

    (** Replaces all arithmetical operations by new constructors. *)
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

(** Modules including BaseMath define basic math operations.
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
    val (+) : t -> t -> t
    val (-) : t -> t -> t
    val ( * ) : t -> t -> t
    val ( ** ) : t -> int -> t
    val (~-) : t -> t
  end

(** Extends a BaseMath module to get all math methods *)
module MakeMath(Base : BaseMath) : (Math with type t := Base.t) =
  struct
    include Base
    let sum = List.fold_left add zero
    let product = List.fold_left mul one
    let sub t1 t2 = add t1 (neg t2)
    let (+) = add
    let (-) = sub
    let ( * ) = mul
    let ( ** ) = pow
    let (~-) = neg
  end

(** A Polynomial represents a mathematical polynomial *)
module type Polynomial =
  sig
    type t
    module Monomial_ : Monomial
    type monomial = Monomial_.t
    module ScaledMonomial_ : ScaledMonomial
    
    include Evaluable with type t := t
    include Math with type t := t
    include PartialOrder with type t := t
    include Ring with type t := t

    (** Following methods are convenience methods for the creation of polynomials. *)

    val make : (Value.t * monomial) list -> t
    val lift : Value.t -> monomial -> t
    val from_var : Var.t -> t
    val from_constant : Value.t -> t
    val var : string -> t
    val value : int -> t
    val helper : int -> t
    val from_power : Var.t -> int -> t
    val from_monomial : monomial -> t
    val from_coeff_list : Value.t list -> Var.t list -> t
      
    (** Following methods return information over the polynomial. *)

    (** Returns the coefficient of the monomial. *)
    val coeff : monomial -> t -> Value.t
    val coeff_of_var : Var.t -> t -> Value.t

    (** Returns the monomials of the polynomial without the empty monomial. *)
    val monomials : t -> monomial list

    (** Returns the constant of the polynomial. *)
    val constant : t -> Value.t

      (*
    (** Returns a maybe not equivalent polynom where all factors of polynomials are minized but stay in same proportion. *)
    val scale_coefficients : t -> t
       *)
      
    val to_string : t -> string
      
      
    (** Following methods return if the atom has certain properties. *)

    (** Returns if the polynomial is equivalent to a term x^1 for any variable x. *)
    val is_var : t -> bool

    (** Returns if the polynomial is equivalent to a term x^1 + c for any variable x and any constant c. *)      
    val is_var_plus_constant : t -> bool

    (** Returns if the polynomial is equivalent to a term x_1^1 + ... + x_n^1 + c for any variables x_1, ..., x_n and any constant c. *)      
    val is_sum_of_vars_plus_constant : t -> bool

    (** Returns if the polyomial is linear and contains at most one active variable. *)
    val is_univariate_linear : t -> bool

    (** Returns if the value of the polynomial is not be affected by any variable. *)
    val is_const : t -> bool

    (** Returns if the polynomial is linear. *)
    val is_linear : t -> bool

    (** Returns if the value of the polynomial is the constant value zero (0). *)
    val is_zero : t -> bool

    (** Returns if the value of the polynomial is the constant value one (1). *)
    val is_one : t -> bool


    (** Misc *)

    (** Creates a polynomial where every variable for which a value is assigned by the valuation is replaced by this value. *)
    val eval_partial : t -> Valuation_.t -> t

    (** Maps all coefficients to elements from the polynomial. *)
    val instantiate : (Value.t -> t) -> t -> t
      
    (** Substitutes every occurrence of the variable in the polynomial by the replacement polynomial.
        Ignores naming equalities. *)
    val substitute : Var.t -> replacement:t -> t -> t
    
    (** Substitutes every occurrence of the variables in the polynomial by the corresponding replacement polynomial. *)
    val substitute_f : (Var.t -> t) -> t -> t
    
    (** Substitutes every occurrence of the variables in the polynomial by the corresponding replacement polynomial.
        Leaves all variables unchanged which are not in the replacement map.  *)
    val substitute_all : t Map.Make(Var).t -> t -> t


    (** Removes all summands from the polynomial which are equivalent to the monomial. *)
    val delete_monomial : monomial -> t -> t

    (** Returns a simplified version of the polynomial.
        Subsequent calls to simplify will not lead to a further simplification. *)
    val simplify : t -> t

    (** Multiplies the polynomial with a constant value.
        The result is always a polynomial. *)
    val mult_with_const : Value.t -> t -> t

    (** Replaces all arithmetical operations by new constructors. *)
    val fold : const:(Value.t -> 'b) ->
               var:(Var.t -> 'b) ->
               neg:('b -> 'b) ->               
               plus:('b -> 'b -> 'b) ->
               times:('b -> 'b -> 'b) ->
               pow:('b -> int -> 'b) ->
               t -> 'b 

  end

(** This module type defines how functors constructing polynomials have to be defined *)
module type PolynomialFunctor =
  functor (Value : Ring) -> Polynomial with module Value = Value

(** Monadize adds the monadic flatten method to a polynomial. *)
module Monadize(P : PolynomialFunctor)(Value : Ring) = struct
  module Inner = P(Value)
  module Outer = P(P(Value))
  include Outer
  (** Transforms the template polynomial such that all inner values get lifted to the outer polynomial. *)
  (** Example: (2a+b)x + (3a)y - 1 gets transformed to 2ax + bx + 3ay - 1 *)
  let flatten (templatepoly : t): Inner.t =
    Outer.fold ~const:identity ~var:Inner.from_var ~neg:Inner.neg ~plus:Inner.add ~times:Inner.mul ~pow:Inner.pow templatepoly
end
  
(** A MinMaxPolynomial is a polynomial which allows the usage of min and max functions  *)
module type MinMaxPolynomial =
  sig
    type t
    include Evaluable with type t := t
    include Math with type t := t
    include PartialOrder with type t := t
    module Polynomial_ : (Polynomial with module Value = Value)


    (** Following methods are convenience methods for the creation of polynomials. *)

    val of_poly : Polynomial_.t -> t              
    val of_constant : Value.t -> t

    val min : t -> t -> t
    val max : t -> t -> t
    val minimum : t list -> t
    val maximum : t list -> t
    val infinity : t
    val minus_infinity : t
    val exp : Value.t -> t -> t
      
    val to_string : t -> string

    (** Following methods can be used to classify the type of the polynomial. *)

    (** TODO *)
    
    (** Substitutes every occurrence of the variable in the polynomial by the replacement polynomial.
        Ignores naming equalities. *)
    val substitute : Var.t -> replacement:t -> t -> t

    (** Substitutes every occurrence of the variables in the polynomial by the corresponding replacement polynomial.
        Leaves all variables unchanged which are not in the replacement map.  *)
    val substitute_all : t Map.Make(Var).t -> t -> t

    (** Substitutes every occurrence of the variables in the polynomial by the corresponding replacement polynomial. *)
    val substitute_f : (Var.t -> t) -> t -> t
      
    (** Replaces all arithmetical operations by new constructors. *)
    val fold : const:(Value.t -> 'b) ->
               var:(Var.t -> 'b) ->
               neg:('b -> 'b) ->               
               plus:('b -> 'b -> 'b) ->
               times:('b -> 'b -> 'b) ->
               pow:('b -> int -> 'b) ->
               exp:(Value.t -> 'b -> 'b) ->
               min:('b -> 'b -> 'b) -> 
               max:('b -> 'b -> 'b) ->
               inf:'b ->
               t -> 'b 
  end
