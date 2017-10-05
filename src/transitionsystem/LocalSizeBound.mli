open Batteries
   
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
type template =

  (** Always smaller or equal to a constant or the value of a prevariable. Examples: x'=x , x'=y , x'=2
      max [c;x1;...;xn]) *)
  | Equality of int

  (** Always smaller or equal to the value of a prevariable plus a constant. Examples: x'=x+1 , x'=y+2 
      d + max [x1;...;xn] *)
  | AddsConstant of int

  (* TODO We can not use the sum of all variables, if the value of a variable might be negative and not actually used in the transition.  *)
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

(** Returns the constant c of the equality bound, if it is an equality bound. *)
val equality_constant : t -> int Option.t

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
