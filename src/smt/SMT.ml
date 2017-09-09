open Batteries

(** Provides different implementations of SMT solvers *)
   
(** A unified interface for SMT solvers (currently supported: Z3) *)
module type Solver =
  sig
    module Constraint : ConstraintTypes.Constraint

    val satisfiable : Constraint.t -> bool
  end

(** Constructs an SMT solver which uses the microsoft project Z3 *)
module MakeZ3Solver(C : ConstraintTypes.Constraint) : (Solver with module Constraint = C) =
  struct
    module Constraint = C
    module Atom = C.Atom_
    module Polynomial = C.Polynomial_
       
    let context = ref (
                      Z3.mk_context [
                          ("model", "true");
                          ("proof", "false");
                        ]
                    )

    let from_constraint (constraints : Constraint.t) =
      Constraint.fold ~const:(fun value -> Z3.Arithmetic.Integer.mk_numeral_i !context (Polynomial.Value.to_int value))
                      ~var:(fun var -> Z3.Arithmetic.Integer.mk_const_s !context (Polynomial.Var.to_string var))
                      ~neg:(Z3.Arithmetic.mk_unary_minus !context)
                      ~plus:(fun p1 p2 -> Z3.Arithmetic.mk_add !context [p1; p2])
                      ~times:(fun p1 p2 -> Z3.Arithmetic.mk_mul !context [p1; p2])
                      ~pow:(fun b e -> Z3.Arithmetic.mk_power !context b (Z3.Arithmetic.Integer.mk_numeral_i !context e))
                      ~le:(Z3.Arithmetic.mk_le !context)
                      ~correct:(Z3.Boolean.mk_true !context)
                      ~conj:(fun a1 a2 -> Z3.Boolean.mk_and !context [a1; a2])
                      constraints
      
    let satisfiable (constraints : Constraint.t) =
      let solver = Z3.Solver.mk_simple_solver !context in
      let formula = from_constraint constraints in
      Z3.Solver.add solver [formula];
      (Z3.Solver.check solver []) == Z3.Solver.SATISFIABLE

  end
