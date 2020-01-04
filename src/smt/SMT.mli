(** Provides different implementations of SMT solvers *)
open Batteries
open Formulas
open Polynomials
   
(** Provides different implementations of SMT solvers *)
   
(** A unified interface for SMT solvers (currently supported: Z3) *)
module Z3Solver :
sig
    (** Checks if there exists a satisfying assignment for a given formula. *)
    val satisfiable : Formula.t -> bool

    (** Checks if there exists no satisfying assignment for a given formula. *)
    val unsatisfiable : Formula.t -> bool

    (** Checks if every assignment is satisfying for a given formula. *)
    val tautology : Formula.t -> bool
      
    (** Checks if two formulas are equivalent. *)
    val equivalent : Formula.t -> Formula.t -> bool
      
    (** Returns a model of a formula. TODO doc  ?coeffs_to_minimise: *)
    val get_model : ?coeffs_to_minimise:Var.t list -> Formula.t -> Polynomial.valuation Option.t
    
    (** Returns true iff the formula implies the positivity of the polynomial. *)
    val check_positivity : Formula.t -> Polynomial.t -> bool
    
    (** Returns true iff the formula implies the negativity of the polynomial. *)
    val check_negativity : Formula.t -> Polynomial.t -> bool
  end

(** A unified interface for incremental SMT solvers (currently supported: Z3) *)
module IncrementalZ3Solver :
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

  (** Minimizes the variable. *)
  val minimize : t -> Var.t -> unit

  (** Maximizes the variable. *)
  val maximize : t -> Var.t -> unit

  (** Sets the variables, which absolute value should be minimized. *)
  val minimize_absolute : t -> Var.t list -> unit

  (** Returns a model of the current state, if the state is satisfiable. *)
  val model : t -> Polynomial.valuation Option.t

end
