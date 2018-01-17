open Batteries
open Formulas
open Polynomials
   
(** Provides different implementations of SMT solvers *)
   
(** A unified interface for SMT solvers (currently supported: Z3) *)
module Z3Solver :
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

  (** Creates a new incremental smt solver. *)
  val create : unit -> t
     
  (** Creates a backtracking point. *)
  val push : t -> unit

  (** Backtrack one backtracking point. Note that an exception is thrown if Pop is called without a corresponding push. *)
  val pop : t -> unit

  (** Checks if the current state is satisfiable. *)
  val satisfiable : t -> bool

  (** Asserts the formula. *)
  val add : t -> Formula.t -> unit

  (** Sets the variables, which absolute value should be minimized. *)
  val minimize : t -> Var.t list -> unit

  (** Returns a model of the current state, if the state is satisfiable. *)
  val model : t -> Polynomial.valuation Option.t

end
