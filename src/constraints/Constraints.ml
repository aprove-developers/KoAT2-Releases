open Batteries
   
module Make(A : ConstraintTypes.Atom) =
  struct
    
    module A = A
    
    type value = A.value
    type polynomial = A.polynomial
    type atom = A.t
       
    type t = A.t list
    
    (** Same as List.length but outside of this module the list structure of constraints is invisible*)
    let num_of_atoms = List.length
    
    let lift atom = [ atom ]
    
    let mk atoms = atoms

    let mk_true = mk []

    let mk_eq poly1 poly2 =
      mk A.Infix.([poly1 <= poly2; poly2 <= poly1])

    let mk_gt p1 p2 = lift (A.mk_gt p1 p2)
    let mk_ge p1 p2 = lift (A.mk_ge p1 p2)
    let mk_lt p1 p2 = lift (A.mk_lt p1 p2)
    let mk_le p1 p2 = lift (A.mk_le p1 p2)
    
    let mk_and = List.append
                    
    module Infix = struct
      let (=) = mk_eq
      let (>) = mk_gt
      let (>=) = mk_ge 
      let (<) = mk_lt 
      let (<=) = mk_le
      let (&&) = mk_and
    end

    (** TODO Filter related: x < 0 && x < 1*)
    let all = List.flatten 
      
    let is_true = function
      | [] -> true
      | _ -> false
      
    (* TODO Wrong, use SMT-solver here *)
    let (=~=) constr1 constr2 =
         List.combine constr1 constr2
      |> List.map (uncurry A.(=~=))
      |> List.fold_left (&&) true 
            
    let vars constr =
         constr
      |> List.map (A.vars)
      |> List.fold_left Set.union Set.empty
        
    let to_string constr = String.concat " && " (List.map A.to_string constr)
        
    let rename constr varmapping = List.map (fun atom -> A.rename atom varmapping) constr

                                 (*
    let models constr valuation = 
      List.for_all (fun atom -> Atom_.models atom valuation) constr
                                  *)
        
    let fold ~const ~var ~neg ~plus ~times ~pow ~le ~correct ~conj =
      List.fold_left (fun c atom -> conj c (A.fold ~const ~var ~neg ~plus ~times ~pow ~le atom)) correct
      
    let drop_nonlinear constr =
      List.filter A.is_linear constr 

    (**returns a list of the coefficients of a variable in all the left sides of the constraints*)
    let get_coefficient_vector var constr = 
        List.map (fun atom -> A.P.coeff_of_var var (A.normalised_lhs atom)) constr
            
    (**returns a list of the constants of the constraints*)
    let get_constant_vector constr = 
        List.map (fun atom -> A.P.Value.neg (A.P.constant (A.normalised_lhs atom))) constr 
        
    (** returns a list of lists of the coefficients of the constraint*)
    let rec get_matrix vars constr = 
        let variables = Set.elements vars in
            List.map (fun var -> get_coefficient_vector var constr) variables
    
    let dualise vars matrix column =
        let dualised_left = List.map (fun row -> A.P.from_coeff_list row vars) matrix in
            let dualised_eq = List.flatten (List.map2 mk_eq dualised_left column) in
                let ensure_pos = List.map (fun v -> A.Infix.(A.P.from_var v >= A.P.zero)) vars in
                    mk (List.flatten [dualised_eq;ensure_pos])
        
    (** Farkas Lemma applied to a linear constraint and an atom which is the cost function*)    
    let farkas_transform constr atom =
        let vars = Set.union (vars constr) (A.vars atom) in
        let costfunction = lift atom in
            let a_matrix = get_matrix vars constr in
            let b_right = get_constant_vector constr in
            let c_left = List.map (A.P.from_constant) (List.flatten (get_matrix vars costfunction)) in
            let d_right = List.at (get_constant_vector costfunction) 0 in
                let num_of_constr = List.length constr in
                let fresh_vars = Var.fresh_id_list num_of_constr in
                    let dual_constr = dualise fresh_vars a_matrix c_left in
                        let cost_constr = A.P.from_coeff_list b_right fresh_vars in
                            Infix.(dual_constr && cost_constr <= A.P.from_constant d_right)
                                
  end

module PolynomialConstraint =
  struct
    include Make(Atoms.PolynomialAtom)

  end
