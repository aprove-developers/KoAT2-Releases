open OurBase
open Formulas
open Polynomials
open Bounds

exception SMTFailure of string

(** Provides different implementations of SMT solvers *)

(** A unified interface for SMT solvers (currently supported: Z3) *)
module Z3Solver : sig
  val version : string
  (** Version of the Z3 solver *)

  val satisfiable : Formula.t -> bool
  val unsatisfiable : Formula.t -> bool
  val tautology : Formula.t -> bool
  val equivalent : Formula.t -> Formula.t -> bool
  val get_model : ?coeffs_to_minimise:Var.t list -> Formula.t -> Polynomial.valuation Option.t
  val check_positivity : Formula.t -> Polynomial.t -> bool
  val check_negativity : Formula.t -> Polynomial.t -> bool
end

module Z3SolverTimeout : sig
  val version : string
  (** Version of the Z3 solver *)

  val satisfiable : Formula.t -> bool
  val unsatisfiable : Formula.t -> bool
  val tautology : Formula.t -> bool
  val equivalent : Formula.t -> Formula.t -> bool
  val get_model : ?coeffs_to_minimise:Var.t list -> Formula.t -> Polynomial.valuation Option.t
  val check_positivity : Formula.t -> Polynomial.t -> bool
  val check_negativity : Formula.t -> Polynomial.t -> bool
end

(* Incrementel Solver *)
module IncrementalZ3Solver : sig
  type t

  val create : ?model:bool -> unit -> t
  (** Creates a new incremental smt solver. *)

  val push : t -> unit
  (** Creates a backtracking point. *)

  val pop : t -> unit
  (** Backtrack one backtracking point. Note that an exception is thrown if Pop is called without a corresponding push. *)

  val satisfiable : t -> bool
  (** Checks if the current state is satisfiable. *)

  val unsatisfiable : t -> bool
  (** Checks if the current state is satisfiable. *)

  val add : t -> Formula.t -> unit
  (** Asserts the formula. *)

  val add_real : t -> RationalFormula.t -> unit

  val model : t -> Polynomial.valuation Option.t
  (** Returns a model of the current state, if the state is satisfiable. *)

  val model_real : t -> RationalPolynomial.valuation Option.t

  val add_bound_comparison : t -> [ `LE | `LT ] -> Bound.t -> Bound.t -> unit
  (** Add a bound comparison using the specified operator (LE = less-or-equal, LT = less-than)*)

  val add_realbound_comparison : t -> [ `LE | `LT ] -> RationalBound.t -> RationalBound.t -> unit
end
