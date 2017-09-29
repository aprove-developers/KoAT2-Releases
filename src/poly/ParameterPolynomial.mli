open Batteries
module Inner : PolyTypes.Polynomial
module Outer : PolyTypes.Polynomial
type t
module Monomial_ : PolyTypes.Monomial
type monomial = Monomial_.t
module ScaledMonomial_ : PolyTypes.ScaledMonomial

module Value : PolyTypes.Polynomial
module Valuation_ : (PolyTypes.Valuation with type var = Var.t and type value = Value.t)

include PolyTypes.Eq with type t := t
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
include PolyTypes.Math with type t := t
include PolyTypes.PartialOrder with type t := t
include PolyTypes.Ring with type t := t

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

val flatten : Outer.t -> Inner.t