open Batteries
open Program.Types
   
type formula = Formulas.Formula.t

(* Concept:
   Incoming part:
   For UPPER bounds the MAXIMUM of all incoming variables leading to the SCC or a constant bound anywhere in the SCC.
   For LOWER bounds the MINIMUM of all incoming variables leading to the SCC or a constant bound anywhere in the SCC.
   Constant adds:
   For UPPER bounds the sum over all transitions of the SCC, where we differ two cases:
   If there exists a POSITIVE added constant for any of its result variables, then we multiply the HIGHEST constant with the upper runtime bound of the transition.
   If all added constants are negative for all of its result variables, then we multiply the HIGHEST constant (next toward zero) with the lower runtime bound of the transition.
   For LOWER bounds the sum over all transitions of the SCC, where we also differ two cases:
   If there exists a NEGATIVE added constant for any of its result variables, then we multiply the LOWEST constant with the upper runtime bound of the transition.
   If all added constants are POSITIVE for all of its result variables, then we multiply the LOWEST constant (next toward zero) with the lower runtime bound of the transition.
   Scaled sums: upcoming
 *)
             
(** A templated bound is a bound of a certain templated form.
    The different templates are not disjunctive.
    The upcoming template set always includes the previous one. *)
(* TODO We can not use the sum of all variables, if the value of a variable might be negative and not actually used in the transition.  *)
(** Always smaller or equal to a scaling factor multiplied with the sum of all prevariables and a constant. Examples: x'=x+y , x'=2*(x+y+z) 
    s * (e + sum [x1;...;xn]) *)
type t

(** Creates a templated bound from the template and a string list which represents the variable set. *)
val mk : int -> int -> string list -> string list -> t

(** Returns if the templated bounds represent the same bound. *)
val equal : t -> t -> bool

(** Returns the negation of the templated bound. *)
val neg : t -> t

(** Returns the factor of the local sizebound. *)
val factor : t -> int

(** Returns the absolute value of the factor. *)
val abs_factor : t -> int

(** Returns a set of of variables which affect the local sizebound *)
val vars : t -> VarSet.t

(** Returns the constant of the local sizebound. *)
val constant : t -> int
  
(** Converts the templated bound to a string. *)
val to_string : t -> string

(** Converts the templated bound to an actual bound. *)
val as_bound : t Option.t -> Bound.t 

(** Returns a formula which expresses that the variable is smaller or equal to the bound, e.g. x <= b. *)
val as_formula : Var.t -> t -> formula 

(** Checks if the variable is bounded with the templated bound in the formula. *)
val is_bounded_with : Var.t -> formula -> t -> bool

(** Tries to find a templated bound of any of the defined templates. *)
val find_bound : Var.t -> formula -> t Option.t

(** Returns a local sizebound of the specified kind for the variable of the transition. 
    A local sizebound is expressed in relation to the values directly before executing the transition. *)
val sizebound_local : TransitionLabel.kind -> TransitionLabel.t -> Var.t -> t Option.t
  
val sizebound_local_rv : TransitionLabel.kind -> RV.t -> t Option.t
