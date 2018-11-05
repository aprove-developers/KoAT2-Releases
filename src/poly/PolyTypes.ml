open Batteries

(** Provides all module types related to polynomials *)

(** Modules including BasePartialOrder fulfil all requirements to become a partial order.
    They can be typeclass-like extended by MakePartialOrder. *)
module type OurNumber = 
  sig
    include Number.Numeric

    val (=~=) : t -> t -> bool
    val pow : t -> int -> t
    val max : t -> t -> t
    val min : t -> t -> t
  end

module type BasePartialOrder =
  sig
    type t
    val (=~=) : t -> t -> bool
    val (>) : t -> t -> bool Option.t
  end

(** Modules including PartialOrder hold a type that defines a partial order on its elements *)
module type PartialOrder =
  sig
    include BasePartialOrder
    val (<) : t -> t -> bool Option.t
    val (>=) : t -> t -> bool Option.t      
    val (<=) : t -> t -> bool Option.t
  end

(** Extends a BasePartialOrder to get all the methods of a partial order *)
module MakePartialOrder(Base : BasePartialOrder) : (PartialOrder with type t := Base.t) =
  struct
    include Base
    let (>=) b1 b2 =
      if b1 =~= b2 then
        Some true
      else b1 > b2
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

    val is_defined : t -> var -> bool

    (** Returns a list of the variables for which the valuation is defined *)
    val vars : t -> var list

    (** Returns the var to value bindings. *)
    val bindings : t -> (var * value) Enum.t
    
    (** Converts the valuation into a string using the print function*)
    val to_string : t -> string 
  end

(** Evaluable is a unified interface of all parts of a polynomial *)
module type Evaluable =
  sig
    type t
    type value
    type valuation

    val (=~=) : t -> t -> bool       
       
    (** Stable structural equality, but not actual equality *)
    val equal : t -> t -> bool

    (** Stable structural compare, but not an actual compare *)
    val compare : t -> t -> int

    val to_string : t -> string

    (** Returns a set of the variables which occur in the evaluable *)
    val vars : t -> VarSet.t

    (** Assigns each variable inside the polynomial the value of the valuation and returns the arithmetically computed result. 
        !! If the valuation does not provide a value for a variable, an exception is raised. !! *)
    val eval : t -> valuation -> value

    val eval_f : t -> (Var.t -> value) -> value

    (** Assigns the variables of the evaluable new names based on the rename map. *)
    val rename : RenameMap.t -> t -> t

    val degree : t -> int
  end

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
    val fold : const:(value -> 'b) ->
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

    val make : value -> monomial -> t
    val lift : monomial -> t
    val mul : t -> t -> t
    val mult_with_const : value -> t -> t
    val one : t
    val coeff : t -> value
    val monomial : t -> monomial

    val fold : const:(value -> 'b) ->
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
    val sum : t Enum.t -> t
    val product : t Enum.t -> t
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
    let sum = Enum.fold add zero
    let product = Enum.fold mul one
    let sub t1 t2 = add t1 (neg t2)
    let (+) = add
    let (-) = sub
    let ( * ) = mul
    let ( ** ) = pow
    let (~-) = neg
  end

module type Ring =
  sig
    type t [@@deriving eq]

    include BaseMath with type t := t
       
    val equal : t -> t -> bool
    val (=~=) : t -> t -> bool
    (** Stable structural compare, but not an actual compare *)
    val compare : t -> t -> int      

    val of_int : int -> t
    val to_int : t -> int

    val to_string : t -> string
  end
  
(** A Polynomial represents a mathematical polynomial *)
module type Polynomial =
  sig
    type t

    include Evaluable with type t := t
    include Math with type t := t
    include PartialOrder with type t := t
    include Ring with type t := t

    type monomial
    type scaled_monomial
                  
    (** Following methods are convenience methods for the creation of polynomials. *)

    val make : (value * monomial) list -> t
    val lift : value -> monomial -> t
    val of_scaled : scaled_monomial list -> t
    val of_var : Var.t -> t
    val of_constant : value -> t
    val var : string -> t
    val value : int -> t
    val real_helper : int -> t
    val int_helper : int -> t
    val of_power : Var.t -> int -> t
    val of_monomial : monomial -> t
    val of_coeff_list : value list -> Var.t list -> t
      
    (** Following methods return information over the polynomial. *)

    (** Returns the coefficient of the monomial. *)
    val coeff : monomial -> t -> value
    val coeff_of_var : Var.t -> t -> value
      
    (** Returns the monomials of the polynomial without the empty monomial. *)
    val monomials : t -> monomial list

    (** Returns the constant of the polynomial. *)
    val get_constant : t -> value

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
    val eval_partial : t -> valuation -> t

    (** Maps all coefficients to elements from the polynomial. *)
    val instantiate : (value -> t) -> t -> t
      
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

    (** Derives the polynomial with respect to the given variable *)
    val derivative: Var.t -> t -> t

    (** Multiplies the polynomial with a constant value.
        The result is always a polynomial. *)
    val mult_with_const : value -> t -> t

    (** Replaces all arithmetical operations by new constructors. *)
    val fold : const:(value -> 'b) ->
               var:(Var.t -> 'b) ->
               neg:('b -> 'b) ->               
               plus:('b -> 'b -> 'b) ->
               times:('b -> 'b -> 'b) ->
               pow:('b -> int -> 'b) ->
               t -> 'b 

  end
