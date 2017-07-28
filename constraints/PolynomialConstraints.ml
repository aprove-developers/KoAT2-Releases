
open Batteries
open PolyTypes
open ConstraintTypes
module MakePolynomialConstraints(P : Polynomial) =
(*Polynomial Constraints are conjunctions of PolynomialConstraintsAtoms*)
struct
    module PolynomialConstraintsAtoms_ = PolynomialConstraintsAtoms.MakePolynomialConstraintsAtom(P)
        
    type t = PolynomialConstraintsAtoms_.t list
    
    let to_string (constr : t) = String.concat " /\ " (List.map PolynomialConstraintsAtoms_.to_string constr)
        
    let get_variables constr =
            constr
        |> List.map (PolynomialConstraintsAtoms_.get_variables)
        |> List.concat
        |> List.unique
        
    let rename_vars (constr : t) (varmapping : P.RenameMap_.t) = List.map (fun atom -> PolynomialConstraintsAtoms_.rename_vars atom varmapping) constr

    let eval_bool (constr:t) (varmapping : P.Valuation_.t) = 
            constr
        |> List.map (fun atom -> (PolynomialConstraintsAtoms_.eval_bool atom varmapping))
        |> List.fold_left (&&) true
        
    let lift atom = [ atom ]
    
    let mk atoms = atoms

end
