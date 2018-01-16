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
      
    val get_model : ?coeffs_to_minimise:Var.t list -> Formula.t -> Polynomial.valuation
    
    val check_positivity : Formula.t -> Polynomial.t -> bool
    
    val check_negativity : Formula.t -> Polynomial.t -> bool
  end
