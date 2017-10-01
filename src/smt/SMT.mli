open Batteries

(** Provides different implementations of SMT solvers *)
   
(** A unified interface for SMT solvers (currently supported: Z3) *)
module Z3Solver :
sig
    type valuation = Polynomial.Valuation_.t
    type formula = Formula.PolynomialFormula.t

    val satisfiable : formula -> bool

    val unsatisfiable : formula -> bool

    val get_model : formula -> valuation
  end
