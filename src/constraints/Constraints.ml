open Batteries
open Atoms
open Polynomials
   
module ConstraintOver(A : ConstraintTypes.Atom) =
  struct
    
    module A = A
    
    type value = A.value
    type polynomial = A.polynomial
    type compkind = A.compkind
    type atom = A.t
       
    type t = A.t list [@@deriving eq, ord]
    
    (** Same as List.length but outside of this module the list structure of constraints is invisible *)
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
        
    let to_string ?(to_file=false) ?(comp=" <= ") ?(conj=" && ") constr = String.concat conj (List.map (A.to_string ~to_file) constr)
        
    let rename constr varmapping = List.map (fun atom -> A.rename atom varmapping) constr

    let turn =
      List.map A.neg

    let atom_list = identity
      
    let fold ~subject ~le ~lt ~correct ~conj =
      List.fold_left (fun c atom -> conj c (A.fold ~subject ~le ~lt atom)) correct
      
    let map_polynomial f =
      fold ~subject:f ~le:mk_le ~lt:mk_lt ~correct:mk_true ~conj:mk_and
      
    let drop_nonlinear constr =
      List.filter A.is_linear constr
      
    (**returns a list of the coefficients of a variable in all the left sides of the constraints*)
    let get_coefficient_vector var constr = 
        List.map (A.get_coefficient var) constr
            
    (**returns a list of the constants of the constraints*)
    let get_constant_vector constr = 
        List.map A.get_constant constr 

        (** returns a list of lists of the coefficients of the constraint*)
    let get_matrix vars constr =
        List.map (fun var -> get_coefficient_vector var constr) vars
        
    (** returns a list of lists of the coefficients of the constraint*)
    let dualise vars (matrix: A.P.t list list) column =
      let dualised_left = List.map
          (fun row ->
             List.map2 (fun c -> A.P.mul c % A.P.of_var ) row vars
             |> List.enum
             |> A.P.sum)
          matrix
      in
      let dualised_eq = List.flatten (List.map2 mk_eq dualised_left column) in
      let ensure_positivity = List.map (fun v -> A.Infix.(A.P.of_var v >= A.P.zero)) vars in
      mk (List.flatten [dualised_eq; ensure_positivity])
  end

module Constraint =
  struct
    include ConstraintOver(Atom)
      
    let max_of_occurring_constants atoms =
      atoms
      |> List.map Atom.max_of_occurring_constants
      |> List.fold_left OurInt.mul OurInt.one

  end

module ParameterConstraint =
  struct
    include ConstraintOver(ParameterAtom)

    (** Farkas Lemma applied to a linear constraint and a cost function given as System Ax<= b, cx<=d. A,b,c,d are the inputs *)
    let apply_farkas a_matrix b_right c_left d_right =
      let num_of_fresh = List.length b_right in
      let fresh_vars = Var.fresh_id_list Var.Real num_of_fresh in
      let dual_constr = Constraint.dualise fresh_vars a_matrix c_left in
      let cost_constr = Polynomial.of_coeff_list b_right fresh_vars in
      Constraint.Infix.(dual_constr && cost_constr <= d_right)

    (** Invokes farkas quantifier elimination. Uses apply_farkas*)
    let farkas_transform constr param_atom =
      let vars =
        VarSet.union (Constraint.vars constr) (ParameterAtom.vars param_atom)
        |> VarSet.to_list
      in
      let costfunction = lift param_atom in
      let a_matrix = Constraint.get_matrix vars constr in
      let b_right = Constraint.get_constant_vector constr in
      let c_left = List.flatten (get_matrix vars costfunction) in
      (* TODO What, if the list is empty? *)
      let d_right = ParameterAtom.get_constant param_atom in
      apply_farkas (List.map (List.map (Polynomial.of_constant)) a_matrix) b_right c_left d_right
  end

module BoundConstraint =
  struct
    include ConstraintOver(BoundAtom)
end

module RealConstraint =
  struct
    include ConstraintOver(RealAtom)

    let max_of_occurring_constants atoms =
      atoms
      |> List.map RealAtom.max_of_occurring_constants
      |> List.fold_left OurFloat.mul OurFloat.one

    let of_intconstraint intconstraint =
      mk (List.map (fun atom -> RealAtom.of_intatom atom) intconstraint)
  end

module RealParameterConstraint =
  struct
    include ConstraintOver(RealParameterAtom)

    let of_realconstraint =
      RealConstraint.fold
        ~subject:RealParameterPolynomial.of_polynomial
        ~le:mk_le
        ~lt:mk_lt
        ~correct:mk_true
        ~conj:mk_and

    let of_intconstraint =
      of_realconstraint % RealConstraint.of_intconstraint

    let flatten =
      fold
        ~subject:RealParameterPolynomial.flatten
        ~le:RealConstraint.mk_le
        ~lt:RealConstraint.mk_lt
        ~correct:RealConstraint.mk_true
        ~conj:RealConstraint.mk_and

    (**returns a list of the constants of the constraints*)
    let get_constant_vector constr =
        List.map RealParameterAtom.get_constant constr

    (** Farkas Lemma applied to a linear constraint and a cost function given as System Ax<= b, cx<=d. A,b,c,d are the inputs *)
    let apply_farkas (a_matrix: RealPolynomial.t list list) b_right c_left d_right =
      let num_of_fresh = List.length b_right in
      let fresh_vars = Var.fresh_id_list Var.Real num_of_fresh in
      let dual_constr = RealConstraint.dualise fresh_vars a_matrix c_left in
      let cost_constr =
        List.map2 (fun c -> RealPolynomial.mul c % RealPolynomial.of_var) b_right fresh_vars
        |> List.enum
        |> RealPolynomial.sum
      in
      RealConstraint.Infix.(dual_constr && cost_constr <= d_right)

    (** Invokes farkas quantifier elimination. Uses apply_farkas*)
    let farkas_transform (constr: t) (param_atom: RealParameterAtom.t) =
      let vars =
        VarSet.union (vars constr) (RealParameterAtom.vars param_atom)
        |> VarSet.to_list
      in
      let (a_matrix: RealPolynomial.t list list) = get_matrix vars constr in
      let (b_right: RealPolynomial.t list) = get_constant_vector constr in
      let (c_left) = List.map (flip RealParameterAtom.get_coefficient param_atom) vars in
      let d_right = RealParameterAtom.get_constant param_atom in
      apply_farkas a_matrix b_right c_left d_right
  end