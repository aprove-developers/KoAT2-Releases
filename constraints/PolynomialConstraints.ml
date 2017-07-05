(*Polynomial Constraints are conjunctions of PolynomialConstraintsAtoms*)
type t = PolynomialConstraintsAtoms.constraint_atom list

let to_string (constr : t) =
    let atom_strings = List.concat " /\ " (List.map PolynomialConstraintsAtoms.to_string constr)
    
let to_z3 (ctx : Z3.context) (constr : t) =
    mk_and ctx (List.map (PolynomialConstraintsAtoms.to_z3 ctx) constr)