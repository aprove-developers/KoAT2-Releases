open Batteries
open Formulas
open Polynomials

exception SMTFailure of string

module Yices2Solver :
sig
    val satisfiable : Formula.t -> bool

    val unsatisfiable : Formula.t -> bool

    val tautology : Formula.t -> bool

    val equivalent : Formula.t -> Formula.t -> bool

    val get_model : ?coeffs_to_minimise:Var.t list -> Formula.t -> Polynomial.valuation Option.t

    val check_positivity : Formula.t -> Polynomial.t -> bool

    val check_negativity : Formula.t -> Polynomial.t -> bool
  end

module IncrementalYices2Solver:
sig
  type t

  (** Creates a new incremental smt solver. *)
  val create : unit -> t

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

  (** Returns a model of the current state, if the state is satisfiable. *)
  val model : t -> Polynomial.valuation Option.t
end
