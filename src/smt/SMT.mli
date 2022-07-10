open Batteries
open Formulas
open Polynomials
open BoundsInst

exception SMTFailure of string

(** Provides different implementations of SMT solvers *)

(** A unified interface for SMT solvers (currently supported: Z3) *)
module Z3Solver :
sig
    (** Version of the Z3 solver *)
    val version : string

    val satisfiable : Formula.t -> bool

    val unsatisfiable : Formula.t -> bool

    val tautology : Formula.t -> bool

    val equivalent : Formula.t -> Formula.t -> bool

    val get_model : ?coeffs_to_minimise:Var.t list -> Formula.t -> Polynomial.valuation Option.t

    val check_positivity : Formula.t -> Polynomial.t -> bool

    val check_negativity : Formula.t -> Polynomial.t -> bool
  end

(* Incrementel Solver *)
module IncrementalZ3Solver:
sig
  type t

  (** Creates a new incremental smt solver. *)
  val create : ?model:bool -> unit -> t

  (** Creates a backtracking point. *)
  val push : t -> unit

  (** Backtrack one backtracking point. Note that an exception is thrown if Pop is called without a corresponding push. *)
  val pop : t -> unit

  (** Checks if the current state is satisfiable. *)
  val satisfiable : t -> bool

  (** Checks if the current state is satisfiable. *)
  val unsatisfiable : t -> bool

  (** Asserts the formula. *)
  val add : t -> Formula.t -> unit

  val add_real : t -> RealFormula.t -> unit

  (** Returns a model of the current state, if the state is satisfiable. *)
  val model : t -> Polynomial.valuation Option.t

  val model_real : t -> RealPolynomial.valuation Option.t

  (** Add a bound comparison using the specified operator (LE = less-or-equal, LT = less-than)*)
  val add_bound_comparison : t -> [`LE | `LT] -> Bound.t -> Bound.t -> unit

  val add_realbound_comparison : t -> [`LE | `LT] -> RealBound.t -> RealBound.t -> unit
end
