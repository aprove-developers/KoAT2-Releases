open Batteries
open Formulas
open Constraints
open Atoms
open Polynomials
   
module SMTSolver_ = SMT.Z3Solver
module Valuation = Valuation.Make(OurInt)
                  
type t = {
    pol : Program.Location.t -> Polynomial.t (*This should be a parameter Polynomial, so that it can be used a few times*);
    strictly_decreasing : Program.Transition.t list;
    (*non_increasing : Program_.Transition.t list; not necessary as it contains every transition *)
    bounded : Program.Transition.t list;
  }
  
type transitionEnum = Program.Transition.t Enum.t
  
let logger = Logger.make_log "prf"  

let strictly_decreasing f = f.strictly_decreasing
                          
let bounded f = f.bounded

let to_string prf = "The prf orients all transitions weakly and the following transitions strictly\n" ^(String.concat "\n" (List.map Program.Transition.to_string (strictly_decreasing prf) ))^"\n"

let monotonize f =
  raise (Failure "Not yet implemented")
  
(** Farkas Lemma applied to a linear constraint and a cost function given as System Ax<= b, cx<=d. A,b,c,d are the inputs *)
let apply_farkas a_matrix b_right c_left d_right =
  let num_of_fresh = List.length b_right in
  let fresh_vars = Var.fresh_id_list num_of_fresh in
  let dual_constr = Constraint.dualise fresh_vars a_matrix c_left in
  let cost_constr = Polynomial.from_coeff_list b_right fresh_vars in
  Constraint.mk_and dual_constr (Constraint.mk_le cost_constr d_right)
  
(** Invokes farkas quantifier elimination. Uses apply_farkas*)
let farkas_transform constr param_atom =
(*    print_string (String.concat "-> " ["farkas_transform input";(Constraint.to_string constr);ParameterConstraint.to_string (ParameterConstraint.lift param_atom);"\n"]);*)
  let vars = VarSet.union (Constraint.vars constr) (ParameterAtom.vars param_atom) in
  let costfunction = ParameterConstraint.lift param_atom in
  let a_matrix = Constraint.get_matrix vars constr in
  let b_right = Constraint.get_constant_vector constr in
  let c_left = List.flatten (ParameterConstraint.get_matrix vars costfunction) in
  let d_right = List.at (ParameterConstraint.get_constant_vector costfunction) 0 in
  apply_farkas a_matrix b_right c_left d_right
  
(** Given a list of variables an affine template-polynomial is generated*)            
let ranking_template vars =
  let num_vars = (List.length vars) in
  let fresh_coeffs = List.map Polynomial.from_var (Var.fresh_id_list num_vars) in
  let linear_poly = ParameterPolynomial.from_coeff_list fresh_coeffs vars in
  let constant = ParameterPolynomial.from_constant (Polynomial.from_var (List.at (Var.fresh_id_list 1) 0)) in
  ParameterPolynomial.add linear_poly constant 
  
let copy_list_into_hash hashtbl pairs_list =
    let n = List.length pairs_list in
    for i = 0 to n-1 do
      try
        let (first,second) = List.at pairs_list i in
        PrfTable.add hashtbl first second
      with Invalid_argument x-> print_string "Error in copy_list_into_hash"
    done

(**Generates a ranking function template for every location in the program*)
let generate_ranking_template program =
  let generate_ranking_template_ program =
    let vars = VarSet.elements (Program.vars program) in
    let graph = Program.graph program in
    let fresh_table = PrfTable.create (Program.TransitionGraph.nb_vertex graph) in
    let loc_prf = [] in
    let ins_loc_prf = fun vertex-> if Program.Location.(vertex = (Program.start program)) then List.cons (vertex,(ParameterPolynomial.one)) else List.cons (vertex,(ranking_template vars))  in
    let list_of_prf = Program.TransitionGraph.fold_vertex ins_loc_prf graph loc_prf in
    copy_list_into_hash fresh_table list_of_prf;
    fresh_table
  
  in Logger.with_log logger Logger.DEBUG
    (fun () -> "generated_ranking_template", [])
                    ~result:PrfTable.to_string_parapoly
                    (fun () -> generate_ranking_template_ program)
                  
                  
let help_update label var =
  let update_var = TransitionLabel.update label var in
  match update_var with
  |None -> ParameterPolynomial.from_var var
  |Some p -> ParameterPolynomial.from_polynomial p
           
let help_non_increasing (initial : bool) (table : PrfTable.parameter_table) (trans : Program.TransitionGraph.E.t) (vars : Var.t list) =
  if initial then 
                Constraint.mk_true 
              else
                let trans_label = Program.TransitionGraph.E.label trans in
                let start_parapoly = PrfTable.find table (Program.TransitionGraph.E.src trans) in
              (*    print_string (String.concat"-> "["help_non_increasing_start_parapoly";(ParameterPolynomial.to_string start_parapoly);"\n"]);*)
                let target_parapoly = PrfTable.find table (Program.TransitionGraph.E.dst trans) in
              (*    print_string (String.concat"-> "["help_non_increasing_target_parapoly";(ParameterPolynomial.to_string target_parapoly);"\n"]);*)
                let guard = TransitionLabel.guard trans_label in
                let updated_target = ParameterPolynomial.substitute_f (help_update trans_label) target_parapoly in
              (*    print_string (String.concat"-> "["help_non_increasing_updated_target";(ParameterPolynomial.to_string updated_target);"\n"]);*)
                let new_atom = ParameterAtom.mk_ge start_parapoly updated_target in
              (*    print_string (String.concat"-> "["help_non_increasing_input_farkas";(ParameterConstraint.to_string (ParameterConstraint.lift new_atom));"\n"]);*)
                let result = farkas_transform guard new_atom in
                  (*print_string (String.concat"-> "["help_non_increasing_output";(Constraint.to_string result);"\n"]);*)
                result
  
let help_strict_decrease (initial : bool) (table : PrfTable.parameter_table) (trans : Program.TransitionGraph.E.t) (vars : Var.t list) =
  if initial  then 
                  Constraint.mk_true 
  else
    let trans_label = Program.TransitionGraph.E.label trans in
    let start_parapoly = PrfTable.find table (Program.TransitionGraph.E.src trans) in
    let target_parapoly = PrfTable.find table (Program.TransitionGraph.E.dst trans) in
    let guard = TransitionLabel.guard trans_label in
    let updated_target = ParameterPolynomial.substitute_f (help_update trans_label) target_parapoly in
    let cost = ParameterPolynomial.from_polynomial (TransitionLabel.cost trans_label) in
      let new_atom = ParameterAtom.mk_ge start_parapoly ParameterPolynomial.(cost + updated_target) in (*here's the difference*)
      farkas_transform guard new_atom
  
let help_boundedness (initial : bool) (table : PrfTable.parameter_table) (trans : Program.TransitionGraph.E.t) (vars : Var.t list) =
  if initial  then 
                  Constraint.mk_true 
  else
    let trans_label = Program.TransitionGraph.E.label trans in
    let start_parapoly = PrfTable.find table (Program.TransitionGraph.E.src trans) in
    let guard = TransitionLabel.guard trans_label in
    let cost = ParameterPolynomial.from_polynomial (TransitionLabel.cost trans_label) in
      let new_atom = ParameterAtom.mk_ge start_parapoly cost in 
      farkas_transform guard new_atom
  
(**Generates the constraints due to the non increase rule of a polynomial ranking function*)
let get_non_increase_constraints (table : PrfTable.parameter_table) (program : Program.t) (transitions : transitionEnum) =
  let get_non_increase_constraints_ table program transitions=
    let vars = VarSet.elements (Program.vars program) in
    Enum.fold (fun constr -> fun trans -> Constraint.mk_and constr (help_non_increasing (Program.is_initial program trans) table trans vars) ) Constraint.mk_true transitions 
  in Logger.with_log logger Logger.DEBUG
    (fun () -> "determined non_incr constraints", [])
    ~result: Constraint.to_string
    (fun () -> get_non_increase_constraints_ table program transitions)

let help_strict_oriented program table vars trans (smt,bounded) =
      print_string "Entering help_strict_oriented\n";
  let initial = Program.is_initial program trans in
  let curr_smt = Constraint.mk_and smt (help_boundedness initial table trans vars) in
  let curr_smt = Constraint.mk_and curr_smt (help_strict_decrease initial table trans vars) in
      print_string ("help_strict_decrease:curr_smt = "^(Constraint.to_string curr_smt)^"\n");
    let sol = SMTSolver_.satisfiable (Formula.mk curr_smt) in
      if (sol && not(initial)) then
        (curr_smt, List.append bounded [trans])
      else (smt,bounded)
    
let build_strict_oriented (table : PrfTable.parameter_table) (program : Program.t) (transitions:transitionEnum) (non_incr:Constraint.t) =
    print_string "Entering build_strict_oriented\n";
    print_string "Enum_is_empty = ";
    print_string (Bool.to_string (Enum.is_empty transitions));
    print_string "\n";
  let vars = VarSet.elements (Program.vars program) in
    Enum.fold (fun tuple -> fun trans -> help_strict_oriented program table vars trans tuple) (non_incr,[]) transitions
  
let ranking_function_procedure (program : Program.t) (transitions : transitionEnum) =
    print_string "Entering ranking_function_procedure\n";
    print_string "Enum_is_empty = ";
    print_string (Bool.to_string (Enum.is_empty transitions));
    print_string "\n";
  let table = generate_ranking_template program in
    print_string (String.concat "\n" ["ranking_function_procedure"; "Input table"; (PrfTable.to_string_parapoly table); "\n"]);
  let non_incr = get_non_increase_constraints table program transitions in
    print_string "ranking_function_procedure, get_non_increase_constraints is done\n";
    print_string "Enum_is_empty = ";
    print_string (Bool.to_string (Enum.is_empty transitions));
    print_string "\n";
  let (smt_form , bounded) = build_strict_oriented table program transitions non_incr in
    print_string (String.concat "-> " ["final_ranking constraint" ; (Constraint.to_string smt_form); "\n"]);
  let model = SMTSolver_.get_model (Formula.mk smt_form) in
    print_string (String.concat "\n" ["ranking_function_procedure" ; "Z3 has found the ranking table" ; (Valuation.to_string model);"\n"]);
  (PrfTable.map (fun loc prf -> Polynomial.eval_partial (ParameterPolynomial.flatten prf) model) table), bounded
    
let find program =
  let graph = Program.graph program in
    let allTransitions = Program.TransitionGraph.fold_edges_e (fun trans -> List.cons trans) graph [] in
      let transitions = List.enum allTransitions in
        
(*        let find_ program transitions =*)
          let (table,bounded) = ranking_function_procedure program transitions in
          print_string ((PrfTable.to_string_poly table)^"\n");
          {   pol = PrfTable.find table;
              strictly_decreasing = bounded;
              bounded = bounded;
          }
(*    in Logger.with_log logger Logger.DEBUG 
    (fun () -> "Generated Ranking Function",["prf_values", (PrfTable.to_string_poly (Tuple2.first (ranking_function_procedure program transitions)))])
    ~result: to_string
    (fun () -> find_ program transitions)*)