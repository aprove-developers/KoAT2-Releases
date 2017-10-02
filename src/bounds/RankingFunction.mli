open Batteries

(** Provides default implementations of RankingFunctions *)

    module Program_ = Program
    module Constraints_ = Constraints.PolynomialConstraint
    module Polynomial_ = Constraints_.A.P
    module SMTSolver_ = SMT.Z3Solver
    
    type t
    
    (* val fresh_ranking_map: vars list -> location list -> location -> parameterpoly*)

    (** Returns a non-empty list of all transitions which are strictly decreasing and at the same time bounded with one.
        Corresponds to T_> . *)
    val strictly_decreasing : t -> Program_.Transition.t list
   
    (** Returns a non-empty list of all transitions which are bounded below by zero.
        This list also contains all transitions that are strictly decreasing. *)
    val bounded : t -> Program_.Transition.t list

    (** Finds a suitable ranking function which decreases at least one transition and does not increase any transition. *)
    val find : Program_.t -> t

    (** Transforms the ranking function to a monotonic function. *)
    val monotonize : t -> t
    
    (** Invokes Farkas Lemma, to compute a ranking function*)
    val farkas_transform : Constraints_.t -> Atoms.Make(ParameterPolynomial).t -> Constraints_.t
    
    val generate_ranking_template : Program_.t -> (Program_.TransitionGraph.vertex, ParameterPolynomial.t) Hashtbl.t