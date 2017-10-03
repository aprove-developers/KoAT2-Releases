open Batteries

module Constraints_ = Constraints.PolynomialConstraint
module ParameterAtoms_= Atoms.Make(ParameterPolynomial)
module ParameterConstraints_ = Constraints.Make(ParameterAtoms_)
module ParameterFormula_= Formula.Make(ParameterConstraints_)                        
                        
module SMTSolver_ = SMT.Z3Solver
                  
type t = {
    pol : Program.Location.t -> Polynomial.t (*This should be a parameter Polynomial, so that it can be used a few times*);
    strictly_decreasing : Program.Transition.t list;
    (*non_increasing : Program_.Transition.t list; not necessary as it contains every transition *)
    bounded : Program.Transition.t list;
  }

let strictly_decreasing f = f.strictly_decreasing
                          
let bounded f = f.bounded

let monotonize f =
  raise (Failure "Not yet implemented")
  
(** Farkas Lemma applied to a linear constraint and a cost function given as System Ax<= b, cx<=d. A,b,c,d are the inputs *)
let apply_farkas a_matrix b_right c_left d_right =
  let num_of_fresh = List.length b_right in
  let fresh_vars = Var.fresh_id_list num_of_fresh in
  let dual_constr = Constraints_.dualise fresh_vars a_matrix c_left in
  let cost_constr = Polynomial.from_coeff_list b_right fresh_vars in
  Constraints_.mk_and dual_constr (Constraints_.mk_le cost_constr d_right)
  
(** Invokes farkas quantifier elimination. Uses apply_farkas*)
let farkas_transform constr param_atom =
  let vars = VarSet.union (Constraints_.vars constr) (ParameterAtoms_.vars param_atom) in
  let costfunction = ParameterConstraints_.lift param_atom in
  let a_matrix = Constraints_.get_matrix vars constr in
  let b_right = Constraints_.get_constant_vector constr in
  let c_left = List.flatten (ParameterConstraints_.get_matrix vars costfunction) in
  let d_right = List.at (ParameterConstraints_.get_constant_vector costfunction) 0 in
  apply_farkas a_matrix b_right c_left d_right
  
(** Given a list of variables an affine template-polynomial is generated*)            
let ranking_template vars =
  let num_vars = (List.length vars) in
  let fresh_coeffs = List.map Polynomial.from_var (Var.fresh_id_list num_vars) in
  let linear_poly = ParameterPolynomial.from_coeff_list fresh_coeffs vars in
  let constant = ParameterPolynomial.from_constant (Polynomial.from_var (List.at (Var.fresh_id_list 1) 1)) in
  ParameterPolynomial.add linear_poly constant 
  
let copy_list_into_hash hashtbl pairs_list =
  let n = List.length pairs_list in
  for i = 1 to n do
    let (first,second) = List.at pairs_list i in
    Hashtbl.add hashtbl first second
  done;;
  
let generate_ranking_template program =
  let vars = VarSet.elements (Program.vars program) in
  let graph = Program.graph program in
  let fresh_table = Hashtbl.create (Program.TransitionGraph.nb_vertex graph) in
  let loc_prf = [] in
  let ins_loc_prf = fun vertex-> List.cons (vertex,(ranking_template vars))  in
  let list_of_prf = Program.TransitionGraph.fold_vertex ins_loc_prf graph loc_prf in
  copy_list_into_hash fresh_table list_of_prf;
  fresh_table
  
let help_update label var =
  let update_var = TransitionLabel.update label var in
  match update_var with
  |None -> ParameterPolynomial.from_var var
  |Some p -> ParameterPolynomial.from_constant p
           
let help_non_increasing (table : (Program.TransitionGraph.vertex, ParameterPolynomial.t) Hashtbl.t) (trans : Program.TransitionGraph.E.t) (vars : Var.t list) =
  let trans_label = Program.TransitionGraph.E.label trans in
  let start_parapoly = Hashtbl.find table (Program.TransitionGraph.E.src trans) in
  let target_parapoly = Hashtbl.find table (Program.TransitionGraph.E.dst trans) in
  let guard = TransitionLabel.guard trans_label in
  let updated_target = ParameterPolynomial.substitute_f (help_update trans_label) target_parapoly in
  let new_atom = ParameterAtoms_.mk_ge start_parapoly updated_target in
  farkas_transform guard new_atom
  
let help_strict_decrease (table : (Program.TransitionGraph.vertex, ParameterPolynomial.t) Hashtbl.t) (trans : Program.TransitionGraph.E.t) (vars : Var.t list) =
  let trans_label = Program.TransitionGraph.E.label trans in
  let start_parapoly = Hashtbl.find table (Program.TransitionGraph.E.src trans) in
  let target_parapoly = Hashtbl.find table (Program.TransitionGraph.E.dst trans) in
  let guard = TransitionLabel.guard trans_label in
  let updated_target = ParameterPolynomial.substitute_f (help_update trans_label) target_parapoly in
  let new_atom = ParameterAtoms_.mk_gt start_parapoly updated_target in (*here's the difference*)
  farkas_transform guard new_atom
  
let help_boundedness (table : (Program.TransitionGraph.vertex, ParameterPolynomial.t) Hashtbl.t) (trans : Program.TransitionGraph.E.t) (vars : Var.t list) =
  let trans_label = Program.TransitionGraph.E.label trans in
  let start_parapoly = Hashtbl.find table (Program.TransitionGraph.E.src trans) in
  let guard = TransitionLabel.guard trans_label in
  let new_atom = ParameterAtoms_.mk_gt start_parapoly ParameterPolynomial.zero in 
  farkas_transform guard new_atom
  
let get_non_increase_constraints (table : (Program.TransitionGraph.vertex, ParameterPolynomial.t) Hashtbl.t) (program : Program.t) =
  let graph = Program.graph program in
  let vars = VarSet.elements (Program.vars program) in
  Program.TransitionGraph.fold_edges_e (fun trans -> Constraints_.mk_and (help_non_increasing table trans vars) ) graph Constraints_.mk_true
  
let get_strict_decrease_constraints (table : (Program.TransitionGraph.vertex, ParameterPolynomial.t) Hashtbl.t) (program : Program.t) (str_decr :Program.Transition.t list) =
  let vars = VarSet.elements (Program.vars program) in
  List.fold_left (fun constr -> (fun trans -> Constraints_.mk_and (help_strict_decrease table trans vars) constr) ) Constraints_.mk_true str_decr
  
let get_boundedness_constraints (table : (Program.TransitionGraph.vertex, ParameterPolynomial.t) Hashtbl.t) (program : Program.t) (bnds :Program.Transition.t list) =
  let vars = VarSet.elements (Program.vars program) in
  List.fold_left (fun constr -> (fun trans -> Constraints_.mk_and (help_boundedness table trans vars) constr)) Constraints_.mk_true bnds
  
let ranking_function_procedure (program : Program.t) =
  let table = generate_ranking_template program in
  let non_incr = get_non_increase_constraints table program in
  let sol_non_incr = SMTSolver_.get_model (Formula.PolynomialFormula.mk non_incr) in
  Hashtbl.map (fun loc -> (fun prf -> Polynomial.eval_partial (ParameterPolynomial.flatten prf) sol_non_incr)) table
  
  
let find program =
  let table = ranking_function_procedure program in
  {   pol = Hashtbl.find table;
      strictly_decreasing = [];
      bounded = [];
  }
