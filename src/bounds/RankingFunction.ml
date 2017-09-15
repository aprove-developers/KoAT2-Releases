open Batteries

module Make(P : ProgramTypes.Program) =
  struct
    module Program_ = P
    module Constraints_ = Program_.Constraint_
    module Polynomial_ = Constraints_.Polynomial_
    module ParameterPolynomial_ = Polynomials.Make(Polynomial_.Var)(Polynomial_)
    module ParameterConstraints_= Constraints.Make(ParameterPolynomial_)
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
                
 end     
    
  
  
