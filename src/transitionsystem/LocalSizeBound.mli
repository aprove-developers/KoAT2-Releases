open Batteries

type formula = Formula.PolynomialFormula.t
   
(** A classified bound is a bound of a certain form.
    The different classifications are not disjunctive.
    The upcoming classification set always includes the previous one. *)
type classification =
  (** Always smaller or equal to a constant or the value of a prevariable. Examples: x'=x , x'=y , x'=2
      max [c;x1;...;xn]) *)
  | Equality of int
  (** Always smaller or equal to the value of a prevariable plus a constant. Examples: x'=x+1 , x'=y+2 
      d + max [x1;...;xn] *)
  | AddsConstant of int
  (** Always smaller or equal to a scaling factor multiplied with the sum of all prevariables and a constant. Examples: x'=x+y , x'=2*(x+y+z) 
      s * (e + sum [x1;...;xn]) *)
  | ScaledSum of int * int
  (** Always smaller or equal to infinity *)
  | Unbound [@@deriving eq, show]
  
type t = classification * Var.t Set.t

val mk : classification -> string list -> t
       
val equal_classification : classification -> classification -> bool

val show_classification : classification -> string

val equal : t -> t -> bool

val to_string : t -> string

(** Converts the classified bound to an actual bound. *)
val as_bound : t -> Bound.t 

(** Returns a formula which expresses that the variable is smaller or equal to the bound *)
val as_formula : Var.t -> t -> formula 

(** Checks if the variable is bounded with the classified bound in the formula. *)
val is_bounded_with : Var.t -> formula -> t -> bool

(** Tries to find a classified bounds of any of the defined forms. *)
val find_bound : Var.t -> formula -> t

(** Returns a local sizebound of the specified kind for the var of the transition. 
    A local sizebound is expressed in relation to the values directly before executing the transition. *)
val sizebound_local : TransitionLabel.kind -> TransitionLabel.t -> Var.t -> Bound.t
