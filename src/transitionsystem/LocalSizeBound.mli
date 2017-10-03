open Batteries

type formula = Formula.PolynomialFormula.t
             
(** A templated bound is a bound of a certain templated form.
    The different templates are not disjunctive.
    The upcoming template set always includes the previous one. *)
type template =
  (** Always smaller or equal to the value of a prevariable. Examples: x'=x , x'=y
      max [x1;...;xn]) *)
  | VarEquality
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

(** A template with a set of variables makes up a templated bound.
    It can be transformed to a real bound, but the way back is not possible. *)
type t = template * VarSet.t

(** Creates a templated bound from the template and a string list which represents the variable set. *)
val mk : template -> string list -> t

(** Returns if the templates are equal. *)
val equal_template : template -> template -> bool

(** Converts the template to a string. *)
val show_template : template -> string

(** Returns if the templated bounds represent the same bound. *)
val equal : t -> t -> bool

(** Converts the templated bound to a string. *)
val to_string : t -> string

(** Converts the templated bound to an actual bound. *)
val as_bound : t -> Bound.t 

(** Returns a formula which expresses that the variable is smaller or equal to the bound, e.g. x <= b. *)
val as_formula : Var.t -> t -> formula 

(** Checks if the variable is bounded with the templated bound in the formula. *)
val is_bounded_with : Var.t -> formula -> t -> bool

(** Tries to find an equality bound with any constant c and a subset of the given variable set. *)
val find_equality_bound : VarSet.t -> Var.t -> formula -> t Option.t

(** Tries to find an addsconstant bound with any constant d and a subset of the given variable set. *)
val find_addsconstant_bound : VarSet.t -> Var.t -> formula -> t Option.t
  
(** Tries to find a templated bound of any of the defined templates. *)
val find_bound : Var.t -> formula -> t

(** Returns a local sizebound of the specified kind for the variable of the transition. 
    A local sizebound is expressed in relation to the values directly before executing the transition. *)
val sizebound_local : TransitionLabel.kind -> TransitionLabel.t -> Var.t -> t
