open Batteries

module Make(P : ProgramTypes.Program) =
  struct
    module Program_ = P
    module Constraints_ = Program_.Constraint_
    module Polynomial_ = Constraints_.Polynomial_
    module ParameterPolynomial_ = Polynomials.Make(P.Constraint_.Polynomial_.Var)(P.Constraint_.Polynomial_)
    module ParameterConstraints_= Constraints.Make(Polynomials.Make(P.Constraint_.Polynomial_.Var)(P.Constraint_.Polynomial_))
    module ParameterAtoms_= ParameterConstraints_.Atom_
    
    type t = {
        pol : Program_.Location.t -> Polynomial_.t (*This should be a parameter Polynomial, so that it can be used a few times*);
        strictly_decreasing : Program_.Transition.t list;
        non_increasing : Program_.Transition.t list;
      }

    let strictly_decreasing f = f.strictly_decreasing

    let non_increasing f = f.non_increasing

    let find program =
      raise (Failure "Not yet implemented")

    let monotonize f =
      raise (Failure "Not yet implemented")
      
    (** Farkas Lemma applied to a linear constraint and a cost function given as System Ax<= b, cx<=d. A,b,c,d are the inputs *)
    let apply_farkas a_matrix b_right c_left d_right =
        let num_of_fresh = List.length b_right in
            let fresh_vars = Polynomial_.Var.fresh_id_list num_of_fresh in
                let dual_constr = ParameterConstraints_.dualise fresh_vars a_matrix c_left in
                    let cost_constr = ParameterPolynomial_.from_coeff_list b_right fresh_vars in
                        ParameterConstraints_.mk_and dual_constr (ParameterConstraints_.mk_le cost_constr (ParameterPolynomial_.from_constant d_right))
                        
    (** Invokes farkas quatifier elimination. Uses apply_farkas*)
    let farkas_transform constr param_atom =
        let vars = Set.union (Constraints_.vars constr) (ParameterAtoms_.vars param_atom) in
        let costfunction = ParameterConstraints_.lift param_atom in
            let a_matrix = List.map (fun xs -> List.map (Polynomial_.from_constant) xs)(Constraints_.get_matrix vars constr) in
            let b_right = List.map (Polynomial_.from_constant)(Constraints_.get_constant_vector constr) in
            let c_left = List.map (ParameterPolynomial_.from_constant) (List.flatten (ParameterConstraints_.get_matrix vars costfunction)) in
            let d_right = List.at (ParameterConstraints_.get_constant_vector costfunction) 0 in
                apply_farkas a_matrix b_right c_left d_right
                
    (** Given a list of variables an affine template-polynomial is generated*)            
    let ranking_template vars =
        let num_vars = (List.length vars) in
            let fresh_coeffs = List.map Polynomial_.from_var (Polynomial_.Var.fresh_id_list num_vars) in
                let linear_poly = ParameterPolynomial_.from_coeff_list fresh_coeffs vars in
                    let constant = ParameterPolynomial_.from_constant (Polynomial_.from_var (List.at (Polynomial_.Var.fresh_id_list 1) 1)) in
                        ParameterPolynomial_.add linear_poly constant 
                        
    let copy_list_into_hash hashtbl pairs_list =
        let n = List.length pairs_list in
            for i = 1 to n do
                let (first,second) = List.at pairs_list i in
                    Hashtbl.add hashtbl first second
            done;;
                        
    let generate_ranking_template program =
        let vars = Set.elements (Program_.vars program) in
            let graph = Program_.graph program in
                let fresh_table = Hashtbl.create (Program_.TransitionGraph.nb_vertex graph) in
                    let loc_prf = [] in
                        let ins_loc_prf = fun vertex-> List.cons (vertex,(ranking_template vars))  in
                            let list_of_prf = Program_.TransitionGraph.fold_vertex ins_loc_prf graph loc_prf in
                                copy_list_into_hash fresh_table list_of_prf;
                                fresh_table
                                
    let help_update label var =
        let update_var = Program_.TransitionLabel.update label var in
            match update_var with
                |None -> ParameterPolynomial_.from_var var
                |Some p -> ParameterPolynomial_.from_constant p
                                
    let help_non_increasing (table : (Program_.TransitionGraph.vertex, ParameterPolynomial_.t) Hashtbl.t) (trans : Program_.TransitionGraph.E.t) (vars : Program_.Constraint_.Polynomial_.Var.t list) =
        let trans_label = Program_.TransitionGraph.E.label trans in
            let start_parapoly = Hashtbl.find table (Program_.TransitionGraph.E.src trans) in
                let target_parapoly = Hashtbl.find table (Program_.TransitionGraph.E.dst trans) in
                    let guard = Program_.TransitionLabel.guard trans_label in
                        let updated_target = ParameterPolynomial_.substitute_f (help_update trans_label) target_parapoly in
                            let new_atom = ParameterAtoms_.mk_ge start_parapoly updated_target in
                                farkas_transform guard new_atom
                            
    let help_strict_decrease (table : (Program_.TransitionGraph.vertex, ParameterPolynomial_.t) Hashtbl.t) (trans : Program_.TransitionGraph.E.t) (vars : Program_.Constraint_.Polynomial_.Var.t list) =
        let trans_label = Program_.TransitionGraph.E.label trans in
            let start_parapoly = Hashtbl.find table (Program_.TransitionGraph.E.src trans) in
                let target_parapoly = Hashtbl.find table (Program_.TransitionGraph.E.dst trans) in
                    let guard = Program_.TransitionLabel.guard trans_label in
                        let updated_target = ParameterPolynomial_.substitute_f (help_update trans_label) target_parapoly in
                            let new_atom = ParameterAtoms_.mk_gt start_parapoly updated_target in (*here's the difference*)
                                farkas_transform guard new_atom
                            
    let help_boundedness (table : (Program_.TransitionGraph.vertex, ParameterPolynomial_.t) Hashtbl.t) (trans : Program_.TransitionGraph.E.t) (vars : Program_.Constraint_.Polynomial_.Var.t list) =
        let trans_label = Program_.TransitionGraph.E.label trans in
            let start_parapoly = Hashtbl.find table (Program_.TransitionGraph.E.src trans) in
                let guard = Program_.TransitionLabel.guard trans_label in
                    let new_atom = ParameterAtoms_.mk_gt start_parapoly ParameterPolynomial_.zero in 
                            farkas_transform guard new_atom
                            
    let get_non_decrease_constraints (table : (Program_.TransitionGraph.vertex, ParameterPolynomial_.t) Hashtbl.t) (program : P.t) =
        let graph = Program_.graph program in
            let vars = Set.elements (Program_.vars program) in
            Program_.TransitionGraph.fold_edges_e (fun trans -> ParameterConstraints_.mk_and (help_non_increasing table trans vars) ) graph ParameterConstraints_.mk_true 

 end     
    
  
  
