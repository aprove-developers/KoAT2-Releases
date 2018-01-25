open Batteries
open Atoms
open Polynomials
   
module ConstraintOver(A : ConstraintTypes.Atom) =
  struct
    
    module A = A
    
    type value = A.value
    type polynomial = A.polynomial
    type atom = A.t
       
    type t = A.t list [@@deriving eq, ord]
    
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
      |> List.fold_left VarSet.union VarSet.empty
        
    let to_string ?(comp=" <= ") ?(conj=" && ") constr = String.concat conj (List.map (A.to_string ~comp) constr)
        
    let rename constr varmapping = List.map (fun atom -> A.rename atom varmapping) constr

    let turn =
      List.map A.neg

    let atom_list = identity
      
    let fold ~subject ~le ~correct ~conj =
      List.fold_left (fun c atom -> conj c (A.fold ~subject ~le atom)) correct
      
    let map_polynomial f =
      fold ~subject:f ~le:mk_le ~correct:mk_true ~conj:mk_and

  end

module Constraint =
  struct
    include ConstraintOver(Atom)

    let drop_nonlinear constr =
      List.filter Atom.is_linear constr 

    (**returns a list of the coefficients of a variable in all the left sides of the constraints*)
    let get_coefficient_vector var constr = 
        List.map (Atom.get_coefficient var) constr
            
    (**returns a list of the constants of the constraints*)
    let get_constant_vector constr = 
        List.map Atom.get_constant constr 
        
    (** returns a list of lists of the coefficients of the constraint*)
    let rec get_matrix vars constr = 
        let variables = VarSet.elements vars in
            List.map (fun var -> get_coefficient_vector var constr) variables
    
    let dualise vars matrix column =
      let dualised_left = List.map (fun row -> Polynomial.of_coeff_list row vars) matrix in
      let dualised_eq = List.flatten (List.map2 mk_eq dualised_left column) in
      let ensure_pos = List.map (fun v -> A.Infix.(Polynomial.of_var v >= Polynomial.zero)) vars in
      mk (List.flatten [dualised_eq;ensure_pos])

    (** Farkas Lemma applied to a linear constraint and an atom which is the cost function*)    
    let farkas_transform constr atom =
      let vars = VarSet.union (vars constr) (A.vars atom) in
      let costfunction = lift atom in
      let a_matrix = get_matrix vars constr in
      let b_right = get_constant_vector constr in
      let c_left = List.map (Polynomial.of_constant) (List.flatten (get_matrix vars costfunction)) in
      let d_right = List.at (get_constant_vector costfunction) 0 in
      let num_of_constr = List.length constr in
      (** These are the lambdas in Farkas Lemma which are assumed to take real values*)
      let fresh_vars = Var.fresh_id_list Var.Real num_of_constr in
      let dual_constr = dualise fresh_vars a_matrix c_left in
      let cost_constr = Polynomial.of_coeff_list b_right fresh_vars in
      Infix.(dual_constr && cost_constr <= Polynomial.of_constant d_right)
      
    let max_of_occurring_constants atoms =
      atoms
      |> List.map Atom.max_of_occurring_constants
      |> List.fold_left OurInt.mul OurInt.one

  end

module ParameterConstraint =
  struct
    include ConstraintOver(ParameterAtom)

    (**returns a list of the coefficients of a variable in all the left sides of the constraints*)
    let get_coefficient_vector var constr = 
        List.map (ParameterAtom.get_coefficient var) constr
            
    (**returns a list of the constants of the constraints*)
    let get_constant_vector constr = 
        List.map ParameterAtom.get_constant constr 
        
    (** returns a list of lists of the coefficients of the constraint*)
    let rec get_matrix vars constr = 
        let variables = VarSet.elements vars in
            List.map (fun var -> get_coefficient_vector var constr) variables
    
  end

module BoundConstraint =
  struct
    include ConstraintOver(BoundAtom)
  end
