open Batteries
open Constraints
open Atoms
open Polynomials
open Program.Types
   
(** Provides default implementations of RankingFunctions *)

type t

(** Returns the ranking polynomial for the specific location. *)
val rank : t -> Location.t -> Polynomial.t

(** Returns a non-empty list of all transitions which are strictly decreasing and at the same time bounded with one.
    Corresponds to T_> . *)
val strictly_decreasing : t -> Transition.t list
  
(** Returns a list of all transitions for which the prf is defined.
    Corresponds to T'. *)
val transitions : t -> Transition.t list

(** Finds a suitable ranking function which decreases at least one transition and does not increase any transition. *)
val find : Program.t -> Approximation.t -> t

(** Finds a suitable ranking function for the given transitions T'. *)
val find_prf : VarSet.t -> Transition.t list -> t

(** Invokes Farkas Lemma, to compute a ranking function*)
val farkas_transform : Constraint.t -> ParameterAtom.t -> Constraint.t
  
(** Generates a ranking function template for every location in the program*)
val generate_ranking_template : VarSet.t -> Location.t list -> (Location.t -> ParameterPolynomial.t) * Var.t list

(** Converts a ranking function into a string*)
val to_string : t -> string

