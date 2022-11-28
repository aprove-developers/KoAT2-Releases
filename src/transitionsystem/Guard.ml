open Batteries

include Constraints.Constraint

let simplify_guard guard =
  (* Only try to simplify the linear part *)
  let (lin_atoms, non_lin_atoms) = List.partition Atoms.Atom.is_linear @@ simplify @@ atom_list guard in
  let not_implied constrs =
    List.filter (fun c -> SMT.Z3Solver.satisfiable
      Formulas.Formula.(mk_and (List.fold_left (fun f c' -> mk_and f (mk @@ Constraints.Constraint.mk [c'])) mk_true constrs)
                       (neg @@ lift @@ [[c]])))
  in

  let rec greed_minimpl_set constr_chosen = function
    | [] -> constr_chosen
    | constr_missing ->
        let (next_constr, constr_missing') =
          List.map (fun c -> c,not_implied (List.cons c constr_chosen) constr_missing) constr_missing
          |> List.min ~cmp:(fun (_,l) (_,l') -> Batteries.compare (List.length l) (List.length l'))
        in
        greed_minimpl_set (List.cons next_constr constr_chosen) constr_missing'
  in
  (* Perform initial check to catch tautologies *)
  mk @@ greed_minimpl_set [] (not_implied [] lin_atoms) @ non_lin_atoms

let to_string_ ?(to_file = false) ?(pretty = false) t =
  if is_true t then ""
  else
    Constraints.Constraint.to_string ~to_file ~pretty ~conj:(if pretty then " ∧ " else " && ") t

let to_string = to_string_ ~to_file:false
let to_file_string = to_string_ ~to_file:true ~pretty:false
