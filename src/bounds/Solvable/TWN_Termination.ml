open Atoms
open Batteries
open Constraints
open Formulas
open PolyExponential
open Polynomials

(* TERMINATION: *)

module SMTSolver = SMT.Z3Solver
module SMTSolverTimeout = SMT.Z3SolverTimeout

module Make(PM: ProgramTypes.ClassicalProgramModules) = struct
  open PM

  module Check_TWN = Check_TWN.Make(PM)
  module Loop = SimpleCycle.Loop(PM)

  exception Non_Terminating of (Transition.t list * Transition.t list)

  let red_lt poly_list =
      let rec constraint_eq_zero i = function
      | [] -> Constraint.mk_true
      | x::xs when i == 0 -> Constraint.mk_true
      | x::xs -> Constraint.(mk_and (mk_eq x Polynomial.zero) (constraint_eq_zero (i - 1) xs))  in
      let rec formula i = function
      | [] -> Formula.mk_false
      | x::xs -> Formula.(mk_or (mk_and (mk_lt x Polynomial.zero) (constraint_eq_zero (i - 1) poly_list |> Formula.mk)) (formula (i + 1) xs))  in
      formula 1 poly_list

  let red_le poly_list =
      let rec constr = function
      | [] -> Constraint.mk_true
      | x::xs -> Constraint.(mk_and (mk_eq x Polynomial.zero) (constr xs))  in
      Formula.(mk_or (red_lt poly_list) (poly_list |> constr |> mk))

  module Valuation = Valuation.Make(OurInt)

  let termination_ ((guard,update): Loop.t) order npe varmap =
    let self_impl, rest = Constraint.mk_true, Constraint.mk_true in
    (* TODO TWNLoop.invariant twn |> List.partition @@ check_update_invariant twn in *)
    let formula =
      Formula.any (
      List.map (fun constr ->
        List.fold_right (fun atom formula ->
            let poly = Atom.poly atom in
            let sub_poly = PE.substitute varmap poly |> PE.remove_frac |> List.map (RationalPolynomial.normalize % Tuple4.second) in
            let formula_poly = if Atom.is_lt atom
                then sub_poly |> red_lt
                else sub_poly |> red_le in Formula.mk_and formula formula_poly |> Formula.simplify)
                (constr |> List.unique ~eq:Atom.equal) Formula.mk_true) (Formula.mk_and guard (Formula.mk rest) |> Formula.constraints)) in
    let model = SMTSolver.get_model (Formula.mk_and (Formula.mk self_impl) formula |> Formula.simplify) in
    (Option.is_none model)
   |> tap (fun bool ->
      TWN_Proofs.proof_append
          FormattedString.(
          mk_str_line ("Termination: " ^ (string_of_bool bool))
          <> mk_str_line "Formula: "
          <> (Formula.to_string_formatted formula |> mk_block)
          |> mk_paragraph);)

  (* For Testing *)
  let termination t =
    let loop = Loop.mk t in
    let order = Check_TWN.check_triangular loop in
    let pe = PE.compute_closed_form (List.map (fun var ->
        let update_var = Loop.update_var loop var in
        var, update_var) order) in
    let npe = PE.normalize pe in
    let varmap = Hashtbl.of_list (List.combine order npe) in
    termination_ loop order npe varmap
end
