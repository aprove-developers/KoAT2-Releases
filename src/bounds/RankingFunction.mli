open Batteries

(** Provides default implementations of RankingFunctions *)

(*module Make(P : ProgramTypes.Program) : BoundTypes.RankingFunction
       with module Program_ = P
        and module Constraints_ = P.Constraint_
        and module Polynomial_ = P.Constraint_.Polynomial_
        and module ParameterPolynomial_ = Polynomials.Make(P.Constraint_.Polynomial_)
        and module ParameterFormula_= Formula.Make(Polynomials.Make(P.Constraint_.Polynomial_))*)
    module Program_ : ProgramTypes.Program
    module Constraints_ : ConstraintTypes.Constraint
    module Polynomial_ : PolyTypes.Polynomial
    module ParameterPolynomial_ : PolyTypes.Polynomial
    module ParameterFormula_ : ConstraintTypes.Formula
    module ParameterConstraints_ : ConstraintTypes.Constraint
    module ParameterAtoms_ : ConstraintTypes.Atom
    module SMTSolver_ : SMT.Solver
    
    type t
    
    (* val fresh_ranking_map: vars list -> location list -> location -> parameterpoly*)

    (** Returns a non-empty list of all transitions which are strictly decreasing and at the same time bounded with one.
        Corresponds to T_> . *)
    val strictly_decreasing : t -> Program_.Transition.t list

    (** Returns a non-empty list of all transitions which are non increasing.
        This list also contains all transitions that are strictly decreasing. *)
    (*val non_increasing : t -> Program_.Transition.t list*)
    
    (** Returns a non-empty list of all transitions which are bounded below by zero.
        This list also contains all transitions that are strictly decreasing. *)
    val bounded : t -> Program_.Transition.t list

    (** Finds a suitable ranking function which decreases at least one transition and does not increase any transition. *)
    val find : Program_.t -> t

    (** Transforms the ranking function to a monotonic function. *)
    val monotonize : t -> t
    
    (** Invokes Farkas Lemma, to compute a ranking function*)
    val farkas_transform : Constraints_.t -> ParameterConstraints_.Atom_.t -> ParameterConstraints_.t
    
    val generate_ranking_template : Program_.t -> (Program_.TransitionGraph.vertex, ParameterPolynomial_.t) Hashtbl.t