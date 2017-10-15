open Batteries
open Constraints
open Atoms
open Polynomials
   
(** Provides default implementations of RankingFunctions *)

type t
   
(* val fresh_ranking_map: vars list -> location list -> location -> parameterpoly*)

(** Returns a non-empty list of all transitions which are strictly decreasing and at the same time bounded with one.
        Corresponds to T_> . *)
val strictly_decreasing : t -> Program.Transition.t list
  
(** Returns a non-empty list of all transitions which are bounded below by zero.
        This list also contains all transitions that are strictly decreasing. *)
val bounded : t -> Program.Transition.t list

(** Finds a suitable ranking function which decreases at least one transition and does not increase any transition. *)
(*val find : Program.t -> t*)

(** Transforms the ranking function to a monotonic function. *)
val monotonize : t -> t
  
(** Invokes Farkas Lemma, to compute a ranking function*)
val farkas_transform : Constraint.t -> ParameterAtom.t -> Constraint.t
  
val generate_ranking_template : Program.t -> PrfTable.parameter_table

(**Converts a ranking function into a string*)
val to_string : t -> string

val ranking_function_procedure : Program.t -> string (*Polynomials.Polynomial.t PrfTable.t *
           Program.TransitionGraph.E.t list*)
           
(*val to_string_prog : Program.t -> string*)

val generate_ranking_template : Program.t -> PrfTable.parameter_table