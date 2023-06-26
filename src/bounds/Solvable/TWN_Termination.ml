open Atoms
open Batteries
open Constraints
open Formulas
open PolyExponential
open Polynomials

let logger = Logging.(get Twn)

(* TERMINATION: *)

module SMTSolver = SMT.Z3Solver

module Make(PM: ProgramTypes.ClassicalProgramModules) = struct
  open PM

  module Check_TWN = Check_TWN.Make(PM)
  module Loop = Loop.Make(PM)

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

  let check_update_invariant (loop: Loop.t) atom =
    let poly = Atom.poly atom in
    let poly_updated = Polynomial.substitute_f (Loop.update_var loop) poly in
    let atom_updated = Atom.mk_lt poly_updated Polynomial.zero in
    SMTSolver.tautology Formula.(implies (mk [atom]) (mk [atom_updated]))

  module Valuation = Valuation.Make(OurInt)

  let termination_ ?(entry = None) ((guard,update): Loop.t) upd_invariant_cand varmap =
    let self_impl =
      if Option.is_some entry then
        let (_,t,_) = Option.get entry in
        let upd_invariant_cand = List.filter (fun atom -> SMTSolver.tautology Formula.(implies (mk @@ TransitionLabel.guard t) (mk @@ Constraint.mk [atom]))) upd_invariant_cand in
        List.filter (check_update_invariant (guard,update)) (TransitionLabel.guard t)@(upd_invariant_cand)
      else
        Constraint.mk_true in
    let formula =
      Formula.any (
      List.map (fun constr ->
        List.fold_right (fun atom formula ->
            let poly = Atom.poly atom in
            let sub_poly = PE.substitute varmap poly |> PE.remove_frac |> List.map (RationalPolynomial.normalize % Tuple4.second) in
            let formula_poly = if Atom.is_lt atom
                then sub_poly |> red_lt
                else sub_poly |> red_le in Formula.mk_and formula formula_poly |> Formula.simplify)
                (constr |> List.unique ~eq:Atom.equal) Formula.mk_true) (Formula.constraints guard)) in
    let model = SMTSolver.get_model (Formula.mk_and (Formula.mk self_impl) formula |> Formula.simplify) in
    (Option.is_none model)
   |> tap (fun bool ->
      Logger.log logger Logger.INFO (fun () -> "termination", ["is_satisfiable", Bool.to_string (not bool)]);
                      Logger.log logger Logger.DEBUG (fun () -> "termination", ["formula", Formula.to_string formula]);
                      if Option.is_some model then Logger.log logger Logger.DEBUG (fun () -> "termination", ["model", (Option.get model |> Valuation.to_string)]);
      TWN_Proofs.proof_append
          FormattedString.(
          mk_str_line ("Termination: " ^ (string_of_bool bool))
          <> mk_str_line "Formula: "
          <> (Formula.to_string_formatted formula |> mk_block)
          |> mk_paragraph);)

  (* For Testing *)
  let termination (_,t,_) =
    let loop = Loop.mk t in
    let order = Check_TWN.check_triangular loop in
    let pe = PE.compute_closed_form (List.map (fun var ->
        let update_var = Loop.update_var loop var in
        var, update_var) order) in
    let npe = PE.normalize pe in
    let varmap = Hashtbl.of_list (List.combine order npe) in
    termination_ loop [] varmap
end
