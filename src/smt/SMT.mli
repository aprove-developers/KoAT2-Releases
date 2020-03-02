open Batteries
open Formulas
open Polynomials
open BoundsInst

(** Provides different implementations of SMT solvers *)

(* Z3Solver without optimization *)
module Z3Solver :
  sig
    val satisfiable : RealFormula.t -> bool
    val to_string : RealFormula.t -> string
    val bound_gt_zero : RealFormula.t -> RealBound.t -> bool
    val bound_lt_zero : RealFormula.t -> RealBound.t -> bool
    val cmp_bounds: RealFormula.t -> [`GE | `GT] -> RealBound.t -> RealBound.t -> bool
  end

(** A unified interface for SMT solvers (currently supported: Z3) *)
module Z3Opt :
sig
    val satisfiable : Formula.t -> bool

    val unsatisfiable : Formula.t -> bool

    val tautology : Formula.t -> bool

    val equivalent : Formula.t -> Formula.t -> bool

    val get_model : ?coeffs_to_minimise:Var.t list -> Formula.t -> Polynomial.valuation Option.t

    val check_positivity : Formula.t -> Polynomial.t -> bool

    val check_negativity : Formula.t -> Polynomial.t -> bool
  end

module IncrementalZ3Solver :
sig
  type t

  (* Outputs a string with smt2 format for testing *)
  val to_string : t -> string
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

  val  add_real : t -> RealFormula.t -> unit

  (** Minimizes the variable. *)
  val minimize : t -> Var.t -> unit

  (** Maximizes the variable. *)
  val maximize : t -> Var.t -> unit

  (** Sets the variables, which absolute value should be minimized. Does not work as intended? *)
  val minimize_absolute_with_weight : t -> (Var.t*OurFloat.t) list -> unit

  val minimize_absolute : t -> Var.t list -> unit

  (** Sets the variables, which absolute value should be minimized. Another try*)
  val minimize_absolute_iteratively : t -> Var.t list -> unit

  (** Returns a model of the current state, if the state is satisfiable. *)
  val model : t -> Polynomial.valuation Option.t

  (** Returns a real model of the current state, if the state is satisfiable. *)
  val model_real : t -> RealPolynomial.valuation Option.t
end
