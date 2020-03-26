(** Implementation of bounds, i.e., polynomials, exponential terms and max/min terms.*)
open Batteries
open Polynomials
   
(** A MinMaxPolynomial is a polynomial which allows the usage of min and max functions  *)

type t

include PolyTypes.Evaluable with type t := t with type value = OurInt.t
include PolyTypes.Math with type t := t
include PolyTypes.PartialOrder with type t := t

(** {1  {L Following methods are convenience methods for the creation of bounds.}} *)

(** Creates a bound from a polynomial over OutInt. *)
val of_poly : Polynomial.t -> t   
           
(** Creates a constant bound from a constant OurInt. *)
val of_constant : value -> t

(** Creates a constant bound from an integer. *)
val of_int : int -> t

(** Transforms a bound into an integer. *)
val to_int : t -> int

(** Creates a bound from a Variable. *)
val of_var : Var.t -> t

(** Creates a bound of a string representing a variable. *)
val of_var_string : string -> t
  
(** Returns for two bounds p,q the minimum bound min(p,q). *)
val min : t -> t -> t

(** Returns for two bounds p,q the maximum bound max(p,q). *)
val max : t -> t -> t

(** Returns a bound representing the minimum of all the values.
    Raises an exception, if the enum is empty.
    Use the function infinity for those cases. *)
val minimum : t Enum.t -> t

(** Returns a bound representing the maximum of all the values.
    Raises an exception, if the enum is empty.
    Use the function minus_infinity for those cases. *)
val maximum : t Enum.t -> t
  
(** Returns the infinity bound. *)
val infinity : t

(** Returns the negative infinity bound. *)
val minus_infinity : t

(** Returns for a positive integer OurInt i and a bound b the new bound b^i. *)
val exp : value -> t -> t

(** Returns for a bound b the new (absolute) bound |b| := max(0,b) + max(0,-b)*)
val abs : t -> t

(** Returns for a polynomial bound the maximal occuring constant. TODO doc *)
val max_of_occurring_constants : t -> OurInt.t

(** Returns true iff. a bound is infinite. *)
val is_infinity : t -> bool
  
(** Returns true iff. a bound is negative infinty. *)
val is_minus_infinity : t -> bool

(** Creates a string representing the bound by calling {b show} with complexity enabled. *)
val to_string : t -> string

(** Generates a string from a bound and adds the asymptotic complexity if parameter {i complexity} is not assigned to false. *)
val show : ?complexity:bool -> t -> string

(** Functions to classify the quality of the bound *)

  
(** {1  {L Following methods can be used to classify the type of the bound. }}*)

(** Substitutes every occurrence of the variable in the polynomial by the replacement polynomial.
        Ignores naming equalities. *)
val substitute : Var.t -> replacement:t -> t -> t

(** Substitutes every occurrence of the variables in the polynomial by the corresponding replacement polynomial.
        Leaves all variables unchanged which are not in the replacement map.  *)
val substitute_all : t Map.Make(Var).t -> t -> t

(** Substitutes every occurrence of the variables in the polynomial by the corresponding replacement polynomial. *)
val substitute_f : (Var.t -> t) -> t -> t

(** TODO doc*)
val appr_substitution : [ `Lower | `Upper ] -> lower:(Var.t -> t) -> higher:(Var.t -> t) -> t -> t

(** Replaces all arithmetical operations by new constructors. *)
val fold : const:(value -> 'b) ->
           var:(Var.t -> 'b) ->
           neg:('b -> 'b) ->               
           plus:('b -> 'b -> 'b) ->
           times:('b -> 'b -> 'b) ->
           exp:(value -> 'b -> 'b) ->
           max:('b -> 'b -> 'b) ->
           inf:'b ->
           t -> 'b 

(** TODO doc *)
type complexity =
Inf
 (** Bound is infinite. *)
  | Polynomial of int 
  (** Bound is in asymptotic class O(n^i) *)
  | Exponential of int 
  (** Bound is in corresponding asymptotic class O(2^2^...^n) where the integer value denotes the amount of powers.*)

(** TODO doc where is this method? Returns true iff. two bounds are equal. Or asym. equal?*)
val equal_complexity : complexity -> complexity -> bool

(** Returns string representing asymptotic complexity class. *)
val show_complexity : complexity -> string

(** Returns string representing asymptotic complexity class in the TermComp format. *)
val show_complexity_termcomp : complexity -> string
  
(** Returns an overapproximation of the asymptotic complexity of the given bound. *)
val asymptotic_complexity : t -> complexity

(** Returns true iff the asymptotic complexity is n^1. *)
val is_linear : t -> bool

(** Needed for Atomizable but not yet implemented. *)
val coeff_of_var : Var.t -> t -> value

(** Needed for Atomizable but not yet implemented. *)
val of_coeff_list : value list -> Var.t list -> t

(** Returns the constant of a bound *)
val get_constant : t -> value