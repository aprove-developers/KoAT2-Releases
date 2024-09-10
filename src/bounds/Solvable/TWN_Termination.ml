open Atoms
open Batteries
open Constraints
open Formulas
open PolyExponential
open Polynomials

let logger = Logging.(get Twn)

(* TERMINATION: *)

module SMTSolver = SMT.Z3Solver

module Make (Bound : BoundType.Bound) (PM : ProgramTypes.ClassicalProgramModules) = struct
  module Check_TWN = Check_TWN.Make (Bound) (PM)
  module Loop = Loop.Make (Bound) (PM)

  let red_lt poly_list =
    let rec constraint_eq_zero i = function
      | [] -> Constraint.mk_true
      | x :: xs when i == 0 -> Constraint.mk_true
      | x :: xs -> Constraint.(mk_and (mk_eq x Polynomial.zero) (constraint_eq_zero (i - 1) xs))
    in
    let rec formula i = function
      | [] -> Formula.mk_false
      | x :: xs ->
          Formula.(
            mk_or
              (mk_and (mk_lt x Polynomial.zero) (constraint_eq_zero (i - 1) poly_list |> Formula.mk))
              (formula (i + 1) xs))
    in
    formula 1 poly_list


  let red_le poly_list =
    let rec constr = function
      | [] -> Constraint.mk_true
      | x :: xs -> Constraint.(mk_and (mk_eq x Polynomial.zero) (constr xs))
    in
    Formula.(mk_or (red_lt poly_list) (poly_list |> constr |> mk))


  module Valuation = Valuation.Make (OurInt)

  let termination_ twn_proofs ?(entry = None) ((guard, update) : Loop.t) varmap =
    let formula =
      Formula.any
        (List.map
           (fun constr ->
             List.fold_right
               (fun atom formula ->
                 if Loop.check_update_invariant (guard, update) atom then
                   Formula.mk_and formula (Formula.lift @@ [ Constraint.lift atom ]) |> Formula.simplify
                 else
                   let poly = Atom.poly atom in
                   let sub_poly =
                     RationalPE.substitute varmap (RationalPolynomial.of_intpoly poly)
                     |> RationalPE.remove_frac
                     |> List.map (RationalPolynomial.normalize % Tuple4.second)
                   in
                   let formula_poly = red_le sub_poly in
                   Formula.mk_and formula formula_poly |> Formula.simplify)
               (constr |> List.unique ~eq:Atom.equal)
               Formula.mk_true)
           (Formula.constraints guard))
      |> Formula.simplify
    in
    let model = SMTSolver.get_model formula in
    Option.is_none model
    |> tap (fun bool ->
           Logger.log logger Logger.INFO (fun () ->
               ("termination", [ ("is_satisfiable", Bool.to_string (not bool)) ]));
           Logger.log logger Logger.DEBUG (fun () ->
               ("termination", [ ("formula", Formula.to_string formula) ]));
           if Option.is_some model then
             Logger.log logger Logger.DEBUG (fun () ->
                 ("termination", [ ("model", Option.get model |> Valuation.to_string) ]));
           ProofOutput.LocalProofOutput.add_to_proof twn_proofs @@ fun () ->
           FormattedString.(
             mk_str_line ("Termination: " ^ string_of_bool bool)
             <> mk_str_line "Formula: "
             <> (Formula.to_string_formatted formula |> mk_block)
             |> mk_paragraph))


  (* Counterpart to TWN_Complexity.complexity *)
  let termination twn_proofs ?(entry = None) loop =
    let order = Check_TWN.(unwrap_twn @@ check_triangular loop) in
    let t_ =
      if Check_TWN.check_weakly_negativitiy loop then
        Loop.chain loop
        |> tap (fun loop ->
               Logger.log logger Logger.INFO (fun () -> ("negative", [ ("chained", Loop.to_string loop) ])))
      else
        loop
    in
    Logger.log logger Logger.INFO (fun () ->
        ("order", [ ("order", Util.enum_to_string Var.to_string (List.enum order)) ]));
    ProofOutput.LocalProofOutput.add_to_proof twn_proofs (fun () ->
        FormattedString.mk_str_line @@ "  order: "
        ^ Util.enum_to_string (Var.to_string ~pretty:true) (List.enum order));
    let pe =
      RationalPE.compute_closed_form
        (List.map
           (fun var ->
             let update_var = RationalPolynomial.of_intpoly @@ Loop.update_var t_ var in
             (var, update_var))
           order)
    in
    Logger.log logger Logger.INFO (fun () ->
        ("closed-form", List.combine (List.map Var.to_string order) (List.map RationalPE.to_string pe)));
    ProofOutput.LocalProofOutput.add_to_proof twn_proofs (fun () ->
        FormattedString.(
          mk_str "closed-form:"
          <> (List.combine
                (List.map (Var.to_string ~pretty:true) order)
                (List.map RationalPE.to_string_pretty pe)
             |> List.map (fun (a, b) -> a ^ ": " ^ b)
             |> List.map FormattedString.mk_str_line |> FormattedString.mappend |> FormattedString.mk_block)));
    let npe = RationalPE.normalize pe in
    Logger.log logger Logger.INFO (fun () ->
        ( "constrained-free closed-form",
          List.combine (List.map Var.to_string order) (List.map RationalPE.to_string npe) ));
    let varmap = Hashtbl.of_list @@ List.combine order npe in
    termination_ twn_proofs t_ ~entry varmap
end
