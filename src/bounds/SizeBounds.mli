open Batteries
open Program.Types
   

(** Performs a single improvement step to find better sizebounds for the approximation and updates the approximation. *)
val improve : Program.t -> Approximation.t -> Approximation.t


(** Functions which does not need to be called directly. *)
  
type kind = [ `Lower | `Upper ] [@@deriving show]

(** Improves a whole scc. *)
val improve_scc : Program.t -> RVG.t -> Approximation.t -> RV.t list -> Approximation.t

          
(** Computes a bound for a trivial scc. That is an scc which consists only of one result variable without a loop to itself.
    Corresponds to 'SizeBounds for trivial SCCs'. *)
val compute_trivial_bound : kind -> Program.t -> (kind -> Transition.t -> Var.t -> Bound.t) -> RV.t -> Bound.t

(** Computes a bound for a nontrivial scc. That is an scc which consists of a loop.
    Corresponds to 'SizeBounds for nontrivial SCCs'. *)
val compute_nontrivial_bound : kind -> Program.t -> RVG.t -> (Transition.t -> Bound.t) -> (kind -> Transition.t -> Var.t -> Bound.t) -> RV.t list -> Bound.t

  
(** Returns the maximum of all incoming sizebounds applicated to the local sizebound.
    Corresponds to 'SizeBounds for trivial SCCs':
    S'(alpha) = max(S_l(alpha)(S(t',v_1),...,S(t',v_n)) for all t' in pre(t)) *)
val incoming_bound : kind -> Program.t -> (kind -> Transition.t -> Var.t -> Bound.t) -> Bound.t -> Transition.t -> Bound.t

(** Computes for each transition max(s_alpha for all alpha in C_t) and multiplies the results. *)
val extreme_scaling_factor : kind -> RV.t list -> int

(** Returns all the variables that may influence the given alpha and get changed in the scc. *)
val scc_variables : RVG.t -> RV.t list -> RV.t -> Var.t Enum.t

(** Computes for each transition max(abs(pre(alpha)) intersected with C for all alpha in C_t) and multiplies the results. *)
(* TODO Kind only relevant for only positive and only negative effects. *)
val extreme_affecting_scc_variables : kind -> RVG.t -> RV.t list -> RV.t list -> int

val transition_scaling_factor : kind -> RVG.t -> (Transition.t -> Bound.t) -> RV.t list -> RV.t list -> Bound.t

val overall_scaling_factor : kind -> RVG.t -> (Transition.t -> Bound.t) -> RV.t list -> Bound.t

val incoming_vars_effect : kind -> RVG.t -> (kind -> Transition.t -> Var.t -> Bound.t) -> RV.t list -> VarSet.t -> Transition.t -> RV.t -> Bound.t

val transition_effect : kind -> RVG.t -> (kind -> Transition.t -> Var.t -> Bound.t) -> RV.t list -> RV.t Enum.t -> Transition.t -> Bound.t

val effects : kind -> RVG.t -> (Transition.t -> Bound.t) -> (kind -> Transition.t -> Var.t -> Bound.t) -> RV.t list -> Bound.t
