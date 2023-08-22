open! OurBase
(** Provides a unified interface of the parser and lexer for transition graphs. *)

open Bounds

(** Provides a unified interface of the parser and lexer for transition graphs.
    With this module it is possible to abstract from the details of parsing and lexing *)

(** Constructs a reader for the given transition graph *)

val read_file : ?termination:bool -> string -> Program_.t
(** Reads a file associated to the given path and returns a program. *)

val read_program : string -> Program_.t
val read_program_simple : string -> Program_.t
val read_formula : string -> Formulas.Formula.t
val read_constraint : string -> Constraints.Constraint.t
val read_atom : string -> Atoms.Atom.t
val read_polynomial : string -> Polynomials.Polynomial.t
val read_probability_distribution : string -> ProbabilityDistribution.t
val read_update_element : string -> ProbabilisticProgramModules.UpdateElement.t
val read_bound : string -> Bound.t
val read_general_transitions : string -> ProbabilisticProgramModules.GeneralTransitionSet.t
val read_input : ?termination:bool -> ?rename:bool -> bool -> string -> Program_.t
val read_prog_goal_file : ?rename:bool -> string -> Program_.t * Goal.classical Goal.goal

val read_probabilistic_prog_goal_file :
  string -> ProbabilisticPrograms.ProbabilisticProgram.t * Goal.probabilistic Goal.goal

val read_probabilistic_program : string -> ProbabilisticPrograms.ProbabilisticProgram.t
