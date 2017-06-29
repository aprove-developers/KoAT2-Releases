(*Polynomial Constraints are conjunctions of PolynomialConstraintsAtoms*)
type constraints = PolynomialConstraintsAtoms.constraint_atom list

let to_string (constr : constraints) =
    let atom_strings = List.map