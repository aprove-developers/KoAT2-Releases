open Batteries
open PolyTypes
open ConstraintTypes
   
module Make(P : Polynomial) =
  struct
    module Atom_ = Atoms.Make(P)
        
    type t = Atom_.t list
    

    let lift atom = [ atom ]
    
    let mk atoms = atoms

    let mk_true = mk []


    let is_true = function
      | [] -> true
      | _ -> false
      
    let vars constr =
         constr
      |> List.map (Atom_.vars)
      |> List.fold_left Set.union Set.empty
        
    let to_string constr = String.concat " /\ " (List.map Atom_.to_string constr)
        

    let rename constr varmapping = List.map (fun atom -> Atom_.rename atom varmapping) constr

    let eval_bool constr valuation = 
      List.for_all (fun atom -> Atom_.eval_bool atom valuation) constr
        

    let drop_nonlinear constr =
      List.filter Atom_.is_linear constr 

    let to_less_equal atom =
        match (Atom_.comparator atom) with
            |Atom_.Comparator.LE -> lift atom
            |Atom_.Comparator.LT -> lift (Atom_.remove_strictness atom)
            |Atom_.Comparator.GE -> lift (Atom_.mk (Atom_.Comparator.LE)(Atom_.fst atom)(Atom_.snd atom))
            |Atom_.Comparator.GT -> lift (Atom_.remove_strictness (Atom_.mk (Atom_.Comparator.LT)(Atom_.snd atom)(Atom_.fst atom)))
            |Atom_.Comparator.EQ -> [Atom_.mk (Atom_.Comparator.LE)(Atom_.fst atom)(Atom_.snd atom); Atom_.mk (Atom_.Comparator.LE)(Atom_.snd atom)(Atom_.fst atom)]
      
    (**returns a list of the coefficients of a variable in all the left sides of the constraints*)
    let get_coefficient_vector var constr = 
        let mon = P.Monomial_.lift var 1 in 
            List.map (fun atom ->P.coeff mon (Atom_.fst atom)) constr
  end
