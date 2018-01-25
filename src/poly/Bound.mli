open Batteries
open Polynomials
   
(** A MinMaxPolynomial is a polynomial which allows the usage of min and max functions  *)

type t

include PolyTypes.Evaluable with type t := t with type value = OurInt.t
include PolyTypes.Math with type t := t
include PolyTypes.PartialOrder with type t := t

(** Following methods are convenience methods for the creation of polynomials. *)

val of_poly : Polynomial.t -> t              
val of_constant : value -> t
val of_int : int -> t
val to_int : t -> int
val of_var : Var.t -> t
val of_var_string : string -> t
  
val min : t -> t -> t
val max : t -> t -> t

(** Returns a bound representing the minimum of all the values.
    Raises an exception, if the enum is empty.
    Use the function infinity for those cases. *)
val minimum : t Enum.t -> t
(** Returns a bound representing the maximum of all the values.
    Raises an exception, if the enum is empty.
    Use the function minus_infinity for those cases. *)
val maximum : t Enum.t -> t
  
val infinity : t
val minus_infinity : t
val exp : value -> t -> t
val abs : t -> t

val max_of_occurring_constants : t -> OurInt.t

val is_infinity : t -> bool
  
val is_minus_infinity : t -> bool

val to_string : t -> string

(** Functions to classify the quality of the bound *)

  
(** Following methods can be used to classify the type of the polynomial. *)

(** Substitutes every occurrence of the variable in the polynomial by the replacement polynomial.
        Ignores naming equalities. *)
val substitute : Var.t -> replacement:t -> t -> t

(** Substitutes every occurrence of the variables in the polynomial by the corresponding replacement polynomial.
        Leaves all variables unchanged which are not in the replacement map.  *)
val substitute_all : t Map.Make(Var).t -> t -> t

(** Substitutes every occurrence of the variables in the polynomial by the corresponding replacement polynomial. *)
val substitute_f : (Var.t -> t) -> t -> t

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

type complexity =
  | Inf
  | Polynomial of int
  | Exponential of int

val equal_complexity : complexity -> complexity -> bool

val show_complexity : complexity -> string

val show_complexity_termcomp : complexity -> string
  
(** Returns an overapproximation of the asymptotic complexity of the given bound. *)
val asymptotic_complexity : t -> complexity
