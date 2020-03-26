(** Provides all modules related to polynomials. *)
open Batteries

(** Modules including BasePartialOrder fulfil all requirements to become a partial order.
    They can be typeclass-like extended by MakePartialOrder. *)
module type BasePartialOrder =
  sig
    (** Modules including BasePartialOrder fulfil all requirements to become a partial order.
    They can be typeclass-like extended by MakePartialOrder. *)

    (** Type of elements *)
    type t

    val (=~=) : t -> t -> bool

    (** Less than comparison of two elements of type {i t}. *)
    val (>) : t -> t -> bool Option.t
  end

(** Modules including PartialOrder hold a type that defines a partial order on its elements. *)
module type PartialOrder =
  sig
    (** Modules including PartialOrder hold a type {i t} that defines a partial order on its elements. *)

    include BasePartialOrder

    (** Less than comparison of two elements of type {i t}. *)
    val (<) : t -> t -> bool Option.t

    (** Greater or equal comparison of two elements of type {i t}. *)
    val (>=) : t -> t -> bool Option.t      

    (** Less or equal comparison of two elements of type {i t}. *)
    val (<=) : t -> t -> bool Option.t
  end

(** Extends a BasePartialOrder to get all the methods of a partial order. *)
module MakePartialOrder(Base : BasePartialOrder) : (PartialOrder with type t := Base.t) =
  struct
  (** Extends a BasePartialOrder to get all the methods of a partial order. *)

    include Base
    let (>=) b1 b2 =
      if b1 =~= b2 then
        Some true
      else b1 > b2
    let (<) b1 b2 = Option.map not (b1 >= b2)
    let (<=) b1 b2 = Option.map not (b1 > b2)
  end

(** A valuation is a function which maps from a finite set of variables to values. *)
module type Valuation =
  sig
  (** A valuation is a function which maps from a finite set of variables to values. *)

    type t
    
    (** Type of variables. *)
    type var = Var.t

    (** Type of values. *)
    type value

    (** Creates a valuation from variables to values. *)
    val from : (var * value) list -> t

    (** Creates a valuation from a string (var) to int (value) association list. *)
    val from_native : (string * int) list -> t

    (** Creates a valuation where every variable is assigned the value zero. *)
    val zero : var list -> t

    (** Returns the value of the variable.
        !! If the valuation does not provide a value for a variable, an exception is raised. !! *)
    val eval : var -> t -> value

    (** Returns the value of the variable, if the valuation defines one for it. *)
    val eval_opt : var -> t -> value Option.t

    (** Returns true if variable is defined. *)
    val is_defined : t -> var -> bool

    (** Returns a list of the variables for which the valuation is defined. *)
    val vars : t -> var list

    (** Returns the var to value bindings. *)
    val bindings : t -> (var * value) Enum.t
    
    (** Converts the valuation into a string using the print function. *)
    val to_string : t -> string 
  end

(** Evaluable is a unified interface of all parts of a polynomial. *)
module type Evaluable =
  sig
    (** Evaluable is a unified interface of all parts of a polynomial. *)

    type t
    type value
    type valuation

    val (=~=) : t -> t -> bool       
       
    (** Stable structural equality, but not actual equality *)
    val equal : t -> t -> bool

    (** Stable structural compare, but not an actual compare *)
    val compare : t -> t -> int

    (** Returns a string representing the evaluation. Parameter {i to_file} is used to get a representation with less special characters. *)
    val to_string : ?to_file:bool -> t -> string

    (** Returns a set of the variables which occur in the evaluable *)
    val vars : t -> VarSet.t

    (** Assigns each variable inside the polynomial the value of the valuation and returns the arithmetically computed result. 
        !! If the valuation does not provide a value for a variable, an exception is raised. !! *)
    val eval : t -> valuation -> value

    (** Assigns each variable inside the polynomial the value of the evaluated function and returns the arithmetically computed result. *)
    val eval_f : t -> (Var.t -> value) -> value

    (** Assigns the variables of the evaluable new names based on the rename map. *)
    val rename : RenameMap.t -> t -> t

    (** Returns the degree of a polynomial. *)
    val degree : t -> int
  end

(** A monomial is a a finite product of powers without a constant factor. *)
module type Monomial =
  sig
    (** Provides default implementations of a monomial, i.e.,  a finite product of powers without a constant (e.g.: xy^2, y but not 5xy^2 + 7). *)
    
    type t
    include Evaluable with type t := t

    (** Creates a monomial from a list of variables and their exponents. *)
    val make : (Var.t * int) list -> t

    (** Creates a monomial from a variable and a exponent. *)
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

(** A scaled monomial is a monomial multiplied with a coefficient. *)
module type ScaledMonomial =
  sig
  (** A scaled monomial is a monomial multiplied with a coefficient. *)

    type t
    type monomial
    include Evaluable with type t := t
    include PartialOrder with type t := t

    (** Creates a scaled monomial from a monomial and constant factor. *)
    val make : value -> monomial -> t

    (** Lifts a monomial to a scaled monomial with the constant factor one. *)
    val lift : monomial -> t

    (** Multiplies two scaled monomials. The result is always a scaled monomial. *)
    val mul : t -> t -> t

    (** Multiplies a scaled monomial with a constant factor. *)
    val mult_with_const : value -> t -> t

    (** Returns the constant (and neutral with multiplication) scaled monomial one. *)
    val one : t

    (** Returns the constant factor of the scaled monomial. *)
    val coeff : t -> value

    (** Returns the normalized scaled monomial, i.e., the monomial part of the scaled monomial. *)
    val monomial : t -> monomial

    (** Replaces all arithmetical operations by new constructors. *)
    val fold : const:(value -> 'b) ->
               var:(Var.t -> 'b) ->
               times:('b -> 'b -> 'b) ->
               pow:('b -> int -> 'b) ->
               t -> 'b 
  end

(** Modules including BaseMath define basic math operations.
    They can be typeclass-like extended by MakeMath. *)
module type BaseMath =
  sig
  (** Modules including BaseMath define basic math operations.
    They can be typeclass-like extended by MakeMath. *)

    (** Type of elements. *)
    type t

    (** Returns zero element. *)
    val zero : t

    (** Returns one element. *)
    val one : t

    (** Returns negated element. *)
    val neg : t -> t

    (** Returns sum of two element. *)
    val add : t -> t -> t

    (** Returns product of two element. *)
    val mul : t -> t -> t

    (** Returns element to the power of some provided integer value. *)
    val pow : t -> int -> t
  end

(** Modules including Math hold a type that defines basic math operations on its elements. *)
module type Math =
  sig
  (** Modules including Math hold a type that defines basic math operations on its elements. *)

    include BaseMath

    (** Returns the sum of all enums elements. *)
    val sum : t Enum.t -> t

    (** Returns the product of all enums elements. *)
    val product : t Enum.t -> t

    (** Subtracts the second element from the first one. *)
    val sub : t -> t -> t

    (** Addition of two elements. *)
    val (+) : t -> t -> t

    (** Subtraction of two elements. *)
    val (-) : t -> t -> t

    (** Multiplication of two elements *)
    val ( * ) : t -> t -> t

    (** Raises an element to the power of an integer value. *)
    val ( ** ) : t -> int -> t

    (** Negates element. *)
    val (~-) : t -> t
  end

(** Extends a BaseMath module to get all math methods *)
module MakeMath(Base : BaseMath) : (Math with type t := Base.t) =
  struct
    include Base
  
    (** Returns the sum of all enums elements. *)
    let sum = Enum.fold add zero

    (** Returns the product of all enums elements. *)
    let product = Enum.fold mul one

    (** Subtracts the second element from the first one. *)
    let sub t1 t2 = add t1 (neg t2)

    (** Addition of two elements. *)
    let (+) = add

    (** Subtraction of two elements. *)
    let (-) = sub

    (** Multiplication of two elements *)
    let ( * ) = mul

    (** Raises an element to the power of an integer value. *)
    let ( ** ) = pow

    (** Negates element. *)
    let (~-) = neg
  end

(** Providing the construct for the algebraic structure of a ring. *)
module type Ring =
  sig
  (** Providing the construct for the algebraic structure of a ring. *)

    (** Type of its elements. *)
    type t [@@deriving eq]

    include BaseMath with type t := t
       

    (** TODO doc *)
    val equal : t -> t -> bool
    val (=~=) : t -> t -> bool
    (** Stable structural compare, but not an actual compare *)
    val compare : t -> t -> int      

    (** Creates an element of the ring from an integer value. *)
    val of_int : int -> t

    (** Returns a ring element interpreted as an integer value. *)
    val to_int : t -> int

    (** Returns a string representing an element. *)
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

    (** Type of monomial in polynomial *)
    type monomial

    (** Type of scaled monomial in polynomial *)
    type scaled_monomial
                  
    (** {1 {L Following methods are convenience methods for the creation of polynomials.}} *)

    (** Creates a polynomial as the sum of a list of monomials and integer coefficients. *)
    val make : (value * monomial) list -> t

    (** Lifts a monomial and an integer coefficient to a polynomial. *)
    val lift : value -> monomial -> t

    (** Creates a polynomial as the sum of a list of scaled monomials. *)
    val of_scaled : scaled_monomial list -> t

    (** Lifts a variable to a polynomial. *)
    val of_var : Var.t -> t

    (** Lifts a constant to a polynomial. *)
    val of_constant : value -> t

    (** Lifts a variable to a polynomial. *)
    val var : string -> t

    (** Lifts an integer value to a polynomial. *)
    val value : int -> t

    (** TODO doc *)
    val real_helper : int -> t

    (** TODO doc *)
    val int_helper : int -> t

    (** Lifts a variable to the power of an intger value to a polynomial. *)
    val of_power : Var.t -> int -> t

    (** Lifts a monomial to a polynomial *)
    val of_monomial : monomial -> t

    (** Creates a polynomial from a coefficient list and a corresponding variable list. *)
    val of_coeff_list : value list -> Var.t list -> t
      
    (** {1 {L Following methods return information over the polynomial.}} *)

    (** Returns the coefficient of the monomial. *)
    val coeff : monomial -> t -> value

    (** Returns the coefficient of the term where only the given variable occurs. *)
    val coeff_of_var : Var.t -> t -> value
      
    (** Returns the monomials of the polynomial without the empty monomial. *)
    val monomials : t -> monomial list

    (** Returns the constant of the polynomial. *)
    val get_constant : t -> value

      (*
    (** Returns a maybe not equivalent polynom where all factors of polynomials are minized but stay in same proportion. *)
    val scale_coefficients : t -> t
       *)
      
    (** Returns a string representing the polynomial. *)
    val to_string : t -> string

    (** Returns a string representing polynomial with less special characters. *)
    val to_string_to_file : t -> string

      
    (** {1 {L Following methods return if the atom has certain properties.}} *)

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


    (** {1 {L Misc}} *)

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