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
  
    (* Returns the appropiate constructor for z3 constraint depending on the type of comparation *)
    let get_constructor (comparator : Atom.Comparator.t) = match comparator with
      | Atom.Comparator.GT -> Z3.Arithmetic.mk_gt
      | Atom.Comparator.GE -> Z3.Arithmetic.mk_ge
      | Atom.Comparator.LT -> Z3.Arithmetic.mk_lt
      | Atom.Comparator.LE -> Z3.Arithmetic.mk_le
      | Atom.Comparator.EQ -> Z3.Boolean.mk_eq
      | Atom.Comparator.NEQ -> (fun ctx p1 p2 -> Z3.Boolean.mk_not ctx (Z3.Boolean.mk_eq ctx p1 p2))
      
    (* Converts our representation of constraints to the z3 representation *)
    let from_atom (constr : Atom.t) =
      let constructor = get_constructor (Atom.comparator constr) in
      constructor !context
                  (from_polynomial (Atom.fst constr))
                  (from_polynomial (Atom.snd constr))

    let from_constraint (constraints : Constraint.t) =
        Z3.Boolean.mk_and !context (List.map from_atom constraints)
      
    let satisfiable (constraints : Constraint.t) =
      let solver = Z3.Solver.mk_simple_solver !context in
      let formula = from_constraint constraints in
      Z3.Solver.add solver [formula];
      (Z3.Solver.check solver []) == Z3.Solver.SATISFIABLE

  end
