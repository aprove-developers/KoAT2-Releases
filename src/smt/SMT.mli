open Batteries

(** Provides different implementations of SMT solvers *)
   
(** A unified interface for SMT solvers (currently supported: Z3) *)
module Z3Solver :
sig
    type valuation = Polynomials.Make(PolyTypes.OurInt).Valuation_.t
    type formula = Formula.Make(Polynomials.Make(PolyTypes.OurInt)).t

    val satisfiable : formula -> bool
    
    val get_model : formula -> valuation
  end
