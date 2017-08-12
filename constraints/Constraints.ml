open Batteries
open PolyTypes
open ConstraintTypes
   
module Make(P : Polynomial) =
(*Polynomial Constraints are conjunctions of PolynomialConstraintsAtoms*)
struct
    module Atom_ = Atoms.Make(P)
        
    type t = Atom_.t list
    
    let to_string (constr : t) = String.concat " /\ " (List.map Atom_.to_string constr)
        
    let vars constr =
         constr
      |> List.map (Atom_.vars)
      |> List.fold_left Set.union Set.empty
        
    let rename (constr : t) (varmapping : P.RenameMap_.t) = List.map (fun atom -> Atom_.rename atom varmapping) constr

    let eval_bool (constr:t) (varmapping : P.Valuation_.t) = 
      List.for_all (fun atom -> Atom_.eval_bool atom varmapping) constr
        
    let lift atom = [ atom ]
    
    let mk atoms = atoms

    let mk_true = mk []

    let is_true = function
      | [] -> true
      | _ -> false

end
