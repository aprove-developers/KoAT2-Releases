open Batteries

(** Provides different implementations of SMT solvers *)
   
(** A unified interface for SMT solvers (currently supported: Z3) *)
module Z3Solver :
  sig
    module Polynomial_ = Program.Polynomial_
    module Constraint_ = Program.Constraint_
    module Formula_ = Program.Formula_

    val satisfiable : Formula_.t -> bool
    
    val get_model : Formula_.t -> Polynomial_.Valuation_.t
  end