(** Provides all modules related to polynomials. *)
open OurBase

module type OurNumber =
  sig
    type t

    val zero: t

    val one: t

    val add : t -> t -> t

    val neg: t -> t

    val mul: t -> t -> t

    val equal: t -> t -> bool
    val compare: t -> t -> int

    val of_int: int -> t
    val to_int: t -> int
    val to_string: t -> string

    val abs: t -> t

    val (+): t -> t -> t

    module Compare: sig
      val (>): t -> t -> bool
      val (>=): t -> t -> bool
    end


    val (=~=) : t -> t -> bool
    val pow : t -> int -> t
    val max : t -> t -> t
    val min : t -> t -> t

    val is_integral: t -> bool

    val of_ourint : OurInt.t -> t

    (** The constant representing -1.
      This is not provided by Number.Numeric but is quite useful *)
    val minus_one: t
  end

(** module type for indeterminates in polynomials and monomials *)
module type Indeterminate = sig
  type t

  val compare: t -> t -> int

  val to_string:  ?pretty:bool -> ?to_file:bool -> t -> string

  val is_integral: t -> bool

  val rename: RenameMap.t -> t -> t

  val vars: t -> VarSet.t

  val of_var: Var.t -> t

  val equal: t -> t -> bool

  include Comparator.S with type t := t
end

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
    let (<) b1 b2 = Option.map ~f:not (b1 >= b2)
    let (<=) b1 b2 = Option.map ~f:not (b1 > b2)
  end

(** A valuation is a function which maps from a finite set of variables to values. *)
module type Valuation =
  sig
  (** A valuation is a function which maps from a finite set of indeterminates to values. *)

    type t
    type indeterminate

    (** Type of values. *)
    type value

    (** Creates a valuation from indeterminates to values. *)
    val from : (indeterminate * value) list -> t

    (** Creates a valuation where every indeterminate is assigned the value zero. *)
    val zero : indeterminate list -> t

    (** Returns the value of the indeterminate.
        !! If the valuation does not provide a value for a indeterminate, an exception is raised. !! *)
    val eval : indeterminate -> t -> value

    (** Returns the value of the indeterminate, if the valuation defines one for it. *)
    val eval_opt : indeterminate -> t -> value Option.t

    (** Returns true if indeterminate is defined. *)
    val is_defined : t -> indeterminate -> bool

    (** Returns a list of the indeterminates for which the valuation is defined. *)
    val indeterminates : t -> indeterminate list

    (** Returns a list of the variables for which the valuation is defined. *)
    val vars: t -> Var.t list

    (** Returns the indeterminates to value bindings. *)
    val bindings : t -> (indeterminate * value) Sequence.t

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
    type indeterminate

    val (=~=) : t -> t -> bool

    (** Stable structural equality, but not actual equality *)
    val equal : t -> t -> bool

    (** Stable structural compare, but not an actual compare *)
    val compare : t -> t -> int

    (** Returns a string representing the evaluation. Parameter {i to_file} is used to get a representation with less special characters. *)
    val to_string : ?to_file:bool -> ?pretty:bool -> t -> string

    (* Get all indeterminates occuring in the evaluable *)
    val indeterminates: t -> indeterminate list

    (** Returns a set of the variables which occur in the evaluable *)
    val vars : t -> VarSet.t

    (** Assigns each variable inside the polynomial the value of the valuation and returns the arithmetically computed result.
        !! If the valuation does not provide a value for a variable, an exception is raised. !! *)
    val eval : t -> valuation -> value

    (** Assigns each indeterminate inside the polynomial the value of the evaluated function and returns the arithmetically computed result. *)
    val eval_f : t -> (indeterminate -> value) -> value

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
    type indeterminate
    include Evaluable with type t := t and type indeterminate := indeterminate

    (** Creates a monomial from a list of indeterminates and their exponents. *)
    val make : (indeterminate * int) list -> t

    (* Can the monomial take integral values only, i.e., all vars are of sort Var.Int? *)
    val is_integral: t -> bool

    (** Creates a monomial from a sequence of indeterminates and their exponents. *)
    val of_sequence : (indeterminate * int) Sequence.t -> t

    (** Returns a sequence of all occurring indeterminates with their degree *)
    val to_sequence : t -> (indeterminate * int) Sequence.t

    (** Creates a monomial from an indeterminate and a exponent. *)
    val lift : indeterminate -> int -> t

    (** Creates a monomial from an indeterminate with an exponent of 1  *)
    val of_indeterminate : indeterminate -> t

    (** Creates a monomial from a variable with an exponent of 1  *)
    val of_var : Var.t -> t

    (** Returns the degree of the given indeterminate. *)
    val degree_variable : indeterminate -> t -> int

    (** Deletes all occurences of the given indeterminate. *)
    val delete_indeterminate : indeterminate -> t -> t

    (* Get all vars occuring in the monomial*)
    val vars : t -> VarSet.t

    (** Returns if the monomial is of the simplified form x^1 for any variable x. *)
    val is_univariate_linear : t -> bool

    val pow : t -> int -> t

    (** Multiplies two monomials. The result is always a monomial. *)
    val mul : t -> t -> t

    (** Returns a monomial representing the numeric value one (1). *)
    val one : t

    (** Replaces all arithmetical operations by new constructors. *)
    val fold : const:(value -> 'b) ->
               indeterminate:(indeterminate -> 'b) ->
               times:('b -> 'b -> 'b) ->
               pow:('b -> int -> 'b) ->
               t -> 'b
  end

(** A scaled monomial is a monomial multiplied with a coefficient. *)
module type ScaledMonomial =
  sig
  (** A scaled monomial is a monomial multiplied with a coefficient. *)

    type t
    type indeterminate
    type monomial
    include Evaluable with type t := t and type indeterminate := indeterminate
    include PartialOrder with type t := t

    (** Creates a scaled monomial from a monomial and constant factor. *)
    val make : value -> monomial -> t

    (* Can the scaled monomial take integral values only, i.e., the coefficient is integral and all vars are of sort Var.Int? *)
    val is_integral: t -> bool

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
               indeterminate:(indeterminate -> 'b) ->
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

    (** Returns the sum of all sequence elements. *)
    val sum : t Sequence.t -> t

    (** Returns the product of all sequence elements. *)
    val product : t Sequence.t -> t

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
    let sum = Sequence.fold ~f:add ~init:zero

    (** Returns the product of all enums elements. *)
    let product = Sequence.fold ~f:mul ~init:one

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
    type t

    include BaseMath with type t := t

    (*  is the value integral? *)
    val is_integral: t -> bool


    (** TODO doc *)
    val equal : t -> t -> bool
    val (=~=) : t -> t -> bool

    (** Stable structural compare, but not an actual compare *)
    val compare : t -> t -> int

    (** Creates an element of the ring from an integer value. *)
    val of_int : int -> t

    (** Returns a string representing an element. *)
    val to_string : t -> string
  end

(** A Polynomial represents a mathematical polynomial *)
module type Polynomial =
  sig
    type t

    (* type of indeterminates/variables *)
    type indeterminate

    include Evaluable with type t := t and type indeterminate := indeterminate
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

    (** Lifts a indeterminate/variable to a polynomial. *)
    val of_indeterminate: indeterminate -> t

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

    (** Lifts an indeterminate to the power of an intger value to a polynomial. *)
    val of_power : indeterminate -> int -> t

    (** Lifts a monomial to a polynomial *)
    val of_monomial : monomial -> t

    (** Creates a polynomial from a coefficient list and a corresponding indeterminate list. *)
    val of_coeff_list : value list -> indeterminate list -> t

    (** {1 {L Following methods return information over the polynomial.}} *)

    (** Returns the coefficient of the monomial. *)
    val coeff : monomial -> t -> value

    (** Returns a list of all coefficients. *)
    val coeffs : t -> value list

    (** Returns the coefficient of the term where only the given indeterminate occurs. *)
    val coeff_of_indeterminate : indeterminate -> t -> value

    (** Returns the coefficient of the term where only the given variable occurs. *)
    val coeff_of_var : Var.t -> t -> value

    (** Returns the monomials of the polynomial without the empty monomial. *)
    val monomials : t -> monomial list

    (** Returns the scaled monomials of the polynomial without the empty monomial. *)
    val scaled_monomials : t -> scaled_monomial list

    (** Returns all monomials and their coefficients of the polynomial. *)
    val monomials_with_coeffs : t -> (value * monomial) list

    (** Returns the constant of the polynomial. *)
    val get_constant : t -> value

    (** Returns the unique indeterminate that defines the polynomial if it exists *)
    val get_indeterminate: t -> indeterminate option

      (*
    (** Returns a maybe not equivalent polynom where all factors of polynomials are minized but stay in same proportion. *)
    val scale_coefficients : t -> t
       *)

    (** Returns a string representing the polynomial. *)
    val to_string : t -> string

    val to_string_pretty : t -> string

    (** Returns a string representing polynomial with less special characters. *)
    val to_string_to_file : t -> string


    (** {1 {L Following methods return if the atom has certain properties.}} *)

    (** Returns if the polynomial is equivalent to a term x^1 for any indeterminate x. *)
    val is_indeterminate : t -> bool

    (** Returns if the polynomial is equivalent to a term x^1 + c for any indeterminate x and any constant c. *)
    val is_indeterminate_plus_constant : t -> bool

    (** Returns if the polynomial is equivalent to a term x_1^1 + ... + x_n^1 + c for any indeterminates x_1, ..., x_n and any constant c. *)
    val is_sum_of_indeterminates_plus_constant : t -> bool

    (** Returns if the polyomial is linear and contains at most one active indeterminate. *)
    val is_univariate_linear : t -> bool

    (** Returns if the value of the polynomial is not be affected by any indeterminate. *)
    val is_const : t -> bool

    (** Returns if the polynomial is linear. *)
    val is_linear : t -> bool

    (** Returns true iff each monomial has degree >= 1. *)
    val no_constant_addend : t -> bool

    val indeterminate_only_linear : indeterminate -> t -> bool
    val var_only_linear : Var.t -> t -> bool

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
    val substitute : indeterminate -> replacement:t -> t -> t

    (** Substitutes every occurrence of the variables in the polynomial by the corresponding replacement polynomial. *)
    val substitute_f : (indeterminate -> t) -> t -> t

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
               indeterminate:(indeterminate -> 'b) ->
               neg:('b -> 'b) ->
               plus:('b -> 'b -> 'b) ->
               times:('b -> 'b -> 'b) ->
               pow:('b -> int -> 'b) ->
               t -> 'b

end
