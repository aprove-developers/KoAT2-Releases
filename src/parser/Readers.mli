open Batteries
open BoundsInst
open ExactProgramTypes

(** Provides a unified interface of the parser and lexer for transition graphs.
    With this module it is possible to abstract from the details of parsing and lexing *)

(** Constructs a reader for the given transition graph *)

exception Error of string

val read_file : TransitionLabel.trans_id_counter -> string -> Program.t

val read_goal_file : string -> string

val read_program : TransitionLabel.trans_id_counter -> string -> Program.t

val read_program_simple : TransitionLabel.trans_id_counter -> string -> Program.t

val read_formula : string -> Formulas.Formula.t

val read_constraint : string -> Constraints.Constraint.t

val read_atom : string -> Atoms.Atom.t

val read_polynomial : string -> Polynomials.Polynomial.t

val read_bound : string -> Bound.t

val read_prog_goal_file : TransitionLabel.trans_id_counter -> string -> (Program.t * string)

val read_exact_file : string -> ExactProgram.t
