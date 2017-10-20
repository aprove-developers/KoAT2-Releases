open Batteries
open Polynomials
   
(** A MinMaxPolynomial is a polynomial which allows the usage of min and max functions  *)

type t

include PolyTypes.Evaluable with type t := t with type value = OurInt.t
include PolyTypes.Math with type t := t
include PolyTypes.PartialOrder with type t := t

type polynomial = PolynomialOver(OurInt).t

(** Following methods are convenience methods for the creation of polynomials. *)

val of_poly : polynomial -> t              
val of_constant : value -> t
val of_int : int -> t
val to_int : t -> int
val of_var : Var.t -> t
val of_var_string : string -> t
  
val min : t -> t -> t
val max : t -> t -> t
val minimum : t list -> t
val maximum : t list -> t
val infinity : t
val minus_infinity : t
val exp : value -> t -> t
  
val equal : t -> t -> bool
val to_string : t -> string

(** Following methods can be used to classify the type of the polynomial. *)

(** Substitutes every occurrence of the variable in the polynomial by the replacement polynomial.
        Ignores naming equalities. *)
val substitute : Var.t -> replacement:t -> t -> t

(** Substitutes every occurrence of the variables in the polynomial by the corresponding replacement polynomial.
        Leaves all variables unchanged which are not in the replacement map.  *)
val substitute_all : t Map.Make(Var).t -> t -> t

(** Substitutes every occurrence of the variables in the polynomial by the corresponding replacement polynomial. *)
val substitute_f : (Var.t -> t) -> t -> t
  
(** Replaces all arithmetical operations by new constructors. *)
val fold : const:(value -> 'b) ->
           var:(Var.t -> 'b) ->
           neg:('b -> 'b) ->               
           plus:('b -> 'b -> 'b) ->
           times:('b -> 'b -> 'b) ->
           pow:('b -> int -> 'b) ->
           exp:(value -> 'b -> 'b) ->
           min:('b -> 'b -> 'b) -> 
           max:('b -> 'b -> 'b) ->
           abs:(Var.t -> 'b) -> 
           inf:'b ->
           t -> 'b 
