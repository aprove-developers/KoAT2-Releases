open Batteries
open Formulas
open Polynomials
open Program.Types
   
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
val mk : ?s:int -> ?c:int -> ?pos_abs:(string list) -> ?pos_pure:(string list) -> ?neg_abs:(string list) -> ?neg_pure:(string list) -> [`Lower | `Upper] -> t

(** Returns if the templated bounds represent the same bound. *)
val equal : t -> t -> bool

(** Returns the factor of the local sizebound. *)
val factor : t -> int

(** Returns the constant of the local sizebound. *)
val constant : t -> int
  
(** Returns a set of of variables which affect the local sizebound *)
val vars : t -> VarSet.t

(** Returns a set of all variables which monotonical increasingly or monotonical decreasingly affect the local sizebound *)
(** Those are variables with a positive coefficient or a negative coefficient. *)
val vars_of_sign : [`Pos | `Neg] -> t -> VarSet.t
  
val vars_of_purity : [`Pure | `Abs] -> t -> VarSet.t

val pre_kind : ([`Upper | `Lower] * [`Pos | `Neg]) -> [`Upper | `Lower]
  
(** Converts the templated bound to a string. *)
val to_string : t -> string

(** Takes a function that returns sizebounds for each variable and a local sizebound.
    Returns a bound representing the local sizebound with each variable substituted in a way that the bound is valid. *)
val as_substituted_bound : ([`Lower | `Upper] -> Var.t -> Bound.t) -> t -> Bound.t

(** Converts the templated bound to an actual bound. *)
val as_bound : t -> Bound.t 

val default : [`Lower | `Upper] -> Bound.t
  
(** Returns a formula which expresses that the variable is smaller or equal to the bound, e.g. x <= b. *)
val as_formula : Var.t -> t -> Formula.t

(** Tries to find a templated bound of any of the defined templates. *)
val find_bound : [`Lower | `Upper] -> VarSet.t -> Var.t -> Formula.t -> Polynomial.t -> int -> t

(** Returns a local sizebound of the specified kind for the variable of the transition. 
    A local sizebound is expressed in relation to the values directly before executing the transition. *)
val sizebound_local : [`Lower | `Upper] -> VarSet.t -> TransitionLabel.t -> Var.t -> t Option.t
  
val sizebound_local_rv : [`Lower | `Upper] -> VarSet.t -> RV.t -> t Option.t

(** If for all result variables of the given kind a local sizebound is defined, this function returns a local sizebound function.
    Otherwise it returns None. *)
val sizebound_local_scc : [`Lower | `Upper] -> VarSet.t -> RV.t list -> ([`Lower | `Upper] -> RV.t -> t) Option.t

(** Resets all cached data.
    Useful for testing in the same OCaml instance. *)
val reset : unit -> unit
