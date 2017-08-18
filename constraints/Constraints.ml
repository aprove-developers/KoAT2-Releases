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
      
    let to_less_equal atom =
        match (Atom_.comparator atom) with
            |Atom_.Comparator.LE -> lift atom
            |Atom_.Comparator.LT -> lift (Atom_.remove_strictness atom)
            |Atom_.Comparator.GE -> lift (Atom_.mk (Atom_.Comparator.LE)(Atom_.fst atom)(Atom_.snd atom))
            |Atom_.Comparator.GT -> lift (Atom_.remove_strictness (Atom_.mk (Atom_.Comparator.LT)(Atom_.snd atom)(Atom_.fst atom)))
            |Atom_.Comparator.EQ -> [Atom_.mk (Atom_.Comparator.LE)(Atom_.fst atom)(Atom_.snd atom); Atom_.mk (Atom_.Comparator.LE)(Atom_.snd atom)(Atom_.fst atom)]
            |_ -> (lift atom) (*I don't know what to do with <>*)
      
    let drop_nonlinear constr = List.filter Atom_.is_linear constr 
    
    let drop_not_equal constr = List.filter (fun x -> not (Atom_.is_neq x)) constr
    
    (**returns a list of the coefficients of a variable in all the left sides of the constraints*)
    let get_coefficient_vector var constr = 
        let mon = P.Monomial_.lift var 1 in 
            List.map (fun atom ->P.coeff mon (Atom_.fst atom)) constr

end
