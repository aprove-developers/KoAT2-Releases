open Batteries

module type Solver =
  sig
    module Constraint : ConstraintTypes.PolynomialConstraints

    val satisfiable : Constraint.t -> bool
  end

module Z3Solver : Solver =
  struct
    module Constraint = PolynomialConstraints.MakePolynomialConstraints(ID.StringID)(Number.MakeNumeric(Big_int))
    module Atom = Constraint.PolynomialConstraintsAtoms_
    module Polynomial = Constraint.Polynomial_
       
    let context = ref (Z3.mk_context [
                           ("model", "true");
                           ("proof", "false");
                      ])

    let from_power = function
      | (var, pow) -> Z3.Arithmetic.mk_power !context
                                             (Z3.Arithmetic.Integer.mk_const !context (Z3.Symbol.mk_string !context (Polynomial.Var.to_string var)))
                                             (Z3.Arithmetic.Integer.mk_numeral_i !context pow)

    let from_monomial (mon : (Polynomial.Var.t * int) list) = match mon with
      | [] -> Z3.Arithmetic.Integer.mk_numeral_i !context 1
      | list -> mon
                |> List.map from_power
                |> Z3.Arithmetic.mk_mul !context

    let from_scaled (scaled : Polynomial.Value.t * ((Polynomial.Var.t * int) list)) = match scaled with
      | (coeff, monomial) -> 
         Z3.Arithmetic.mk_mul !context [
                                Z3.Arithmetic.Integer.mk_numeral_s !context (Polynomial.Value.to_string coeff);
                                from_monomial monomial;
                              ]
      
    (* Converts our representation of polynomials to the z3 representation *)
    let from_polynomial (poly : Polynomial.t) =
         poly
      |> Polynomial.simplify
      |> Polynomial.data
      |> (fun p -> match p with
                   | [] -> Z3.Arithmetic.Integer.mk_numeral_i !context 0
                   | list -> list
                             |> List.map from_scaled
                             |> Z3.Arithmetic.mk_add !context)

    (* Returns the appropiate constructor for z3 constraint depending on the type of comparation *)
    let get_constructor (comparator : Atom.comparator) = match comparator with
      | Atom.GT -> Z3.Arithmetic.mk_gt
      | Atom.GE -> Z3.Arithmetic.mk_ge
      | Atom.LT -> Z3.Arithmetic.mk_lt
      | Atom.LE -> Z3.Arithmetic.mk_le
      | Atom.EQ -> Z3.Boolean.mk_eq
      | Atom.NEQ -> (fun ctx p1 p2 -> Z3.Boolean.mk_not ctx (Z3.Boolean.mk_eq ctx p1 p2))
      
    (* Converts our representation of constraints to the z3 representation *)
    let from_atom (constr : Atom.t) =
      let constructor = get_constructor (Atom.get_comparator constr) in
      constructor !context
                  (from_polynomial (Atom.get_first_arg constr))
                  (from_polynomial (Atom.get_second_arg constr))

    let from_constraint (constraints : Constraint.t) =
        Z3.Boolean.mk_and !context (List.map from_atom constraints)
      
    let satisfiable (constraints : Constraint.t) = (Z3.Solver.check (Z3.Solver.mk_simple_solver !context) []) == Z3.Solver.SATISFIABLE

  end
