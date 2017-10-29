open Batteries

(** Provides different implementations of SMT solvers *)
   
(** A unified interface for SMT solvers (currently supported: Z3) *)
module Z3Solver :
sig
    type valuation = Valuation.Make(OurInt).t
    type formula = Formulas.Formula.t

    val satisfiable : formula -> bool

    val unsatisfiable : formula -> bool

    val get_model : formula -> valuation
    
    val get_model_opt : formula -> Var.t list-> valuation
    
  end
