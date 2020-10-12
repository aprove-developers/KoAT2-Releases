open Batteries
open Formulas
open Polynomials
open BoundsInst

(** Provides different implementations of SMT solvers *)

(* Z3Solver without optimization *)
module Z3Solver :
  sig
    val satisfiable : RealFormula.t -> bool
    val unsatisfiable : RealFormula.t -> bool

    val satisfiable_int : Formula.t -> bool
    val unsatisfiable_int : Formula.t -> bool

    (* Shows the internal representation of Z3 *)
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
  (** Creates a new incremental smt solver.
      The first entry of the timeout argument specifies the timeout in s for the Z3 Solver used in
      the satisfiable and unsatisfiable function whereas the second argument determines the timeout for the Z3Optimizer in s*)
  val create : ?model:bool -> ?timeout:float option -> unit -> t

  (** Creates a backtracking point. *)
  val push : t -> unit

  (** Backtrack one backtracking point. Note that an exception is thrown if Pop is called without a corresponding push. *)
  val pop : t -> unit

  (** Checks if the current state is satisfiable. *)
  val satisfiable : t -> bool

  (** Checks if the current state is satisfiable. Return None if unknown (e.g. due to incomplete theory or timeout) *)
  val satisfiable_option : t -> bool option

  (** Checks if the current state is satisfiable. *)
  val unsatisfiable : t -> bool

  (** Checks if the current state is unsatisfiable. Return None if unknown (e.g. due to incomplete theory or timeout) *)
  val unsatisfiable_option : t -> bool option

  (** Asserts the formula. *)
  val add : t -> Formula.t -> unit

  val  add_real : t -> RealFormula.t -> unit

  (** Minimizes the variable. *)
  val minimize : t -> Var.t -> unit

  (** Maximizes the variable. *)
  val maximize : t -> Var.t -> unit

  (* maximizes the number of variables set to 0. If add_as_constraint (defaulting to false) is true
    a backtracking point is created before enforcing the minimal number of set variables.
    Note that we assume that the supplied list of variables is not empty.
    *)
  val minimize_set_vars : t -> ?add_as_constraint:bool -> Var.t list -> unit


  (** Sets the variables, which absolute value should be minimized. Does not work as intended? *)
  val minimize_absolute_with_weight : t -> (Var.t*OurFloat.t) list -> unit

  val minimize_absolute : t -> Var.t list -> unit

  val minimize_absolute_old : t -> Var.t list -> unit

  (** Sets the variables, which absolute value should be minimized. Another try*)
  val minimize_absolute_iteratively : t -> Var.t list -> unit

  (** Similarly to minimise_absolute_iteratively but only works on ints.
      Additionally a manually implemented binary search is employed which leads to significantly improved performance
      but is still very slow compared to minimize_absolute_old *)
  val minimise_absolute_ints_binary_search_iteratively: t -> Var.t list -> unit

  (** Returns a model of the current state, if the state is satisfiable. *)
  val model : ?optimized:bool -> t -> Polynomial.valuation Option.t

  (** Returns a real model of the current state, if the state is satisfiable. *)
  val model_real : ?optimized:bool -> t -> RealPolynomial.valuation Option.t
end
