
open Batteries
open PolyTypes
open ConstraintTypes
module MakePolynomialConstraints(Var : ID) (Value : Number.Numeric) =
(*Polynomial Constraints are conjunctions of PolynomialConstraintsAtoms*)
struct
    
    module Polynomial_ = Polynomials.MakePolynomial(Var)(Value)
    
    module PolynomialConstraintsAtoms_ = PolynomialConstraintsAtoms.MakePolynomialConstraintsAtom(Var)(Value)
    
    module Var = Var
    
    module Value = Value

    type t = PolynomialConstraintsAtoms_.t list
    
    type atom = PolynomialConstraintsAtoms_.t

    let to_string (constr : t) = String.concat " /\ " (List.map PolynomialConstraintsAtoms_.to_string constr)
        
    let get_variables constr =
            constr
        |> List.map (PolynomialConstraintsAtoms_.get_variables)
        |> List.concat
        |> List.unique
        
    let rename_vars (constr : t) (varmapping : Polynomial_.RenameMap_.t) = List.map (fun atom -> PolynomialConstraintsAtoms_.rename_vars atom varmapping) constr

    let eval_bool (constr:t) (varmapping : Polynomial_.Valuation_.t) = 
            constr
        |> List.map (fun atom -> (PolynomialConstraintsAtoms_.eval_bool atom varmapping))
        |> List.fold_left (&&) true
        
    let lift atom = [ atom ]
    
    let mk atoms = atoms

end
