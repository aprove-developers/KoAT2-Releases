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
    module Polynomial = Atom.Polynomial_
       
    let context = ref (
                      Z3.mk_context [
                          ("model", "true");
                          ("proof", "false");
                        ]
                    )

    let from_polynomial =
      Polynomial.fold ~const:(fun value -> Z3.Arithmetic.Integer.mk_numeral_i !context (Polynomial.Value.to_int value))
                      ~var:(fun var -> Z3.Arithmetic.Integer.mk_const_s !context (Polynomial.Var.to_string var))
                      ~neg:(Z3.Arithmetic.mk_unary_minus !context)
                      ~plus:(fun p1 p2 -> Z3.Arithmetic.mk_add !context [p1; p2])
                      ~times:(fun p1 p2 -> Z3.Arithmetic.mk_mul !context [p1; p2])
                      ~pow:(fun b e -> Z3.Arithmetic.mk_power !context b (Z3.Arithmetic.Integer.mk_numeral_i !context e))
      
    (* Converts our representation of constraints to the z3 representation *)
    let from_atom (constr : Atom.t) =
      Z3.Arithmetic.mk_le !context
                  (from_polynomial (Atom.normalised_lhs constr))
                  (Z3.Arithmetic.Integer.mk_numeral_i !context 0)

    let from_constraint (constraints : Constraint.t) =
        Z3.Boolean.mk_and !context (List.map from_atom constraints)
      
    let satisfiable (constraints : Constraint.t) =
      let solver = Z3.Solver.mk_simple_solver !context in
      let formula = from_constraint constraints in
      Z3.Solver.add solver [formula];
      (Z3.Solver.check solver []) == Z3.Solver.SATISFIABLE

  end
