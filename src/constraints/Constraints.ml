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

    let mk_eq poly1 poly2 =
      mk [Atom_.mk_le poly1 poly2; Atom_.mk_le poly2 poly1]

    let mk_gt p1 p2 = lift (Atom_.mk_gt p1 p2)
    let mk_ge p1 p2 = lift (Atom_.mk_ge p1 p2)
    let mk_lt p1 p2 = lift (Atom_.mk_lt p1 p2)
    let mk_le p1 p2 = lift (Atom_.mk_le p1 p2)

                    
    (** TODO Filter related: x < 0 && x < 1*)
    let all = List.flatten 
      
    let is_true = function
      | [] -> true
      | _ -> false
      
    (* TODO Wrong, use SMT-solver here *)
    let (=~=) constr1 constr2 =
         List.combine constr1 constr2
      |> List.map (uncurry Atom_.(=~=))
      |> List.fold_left (&&) true 
            
    let vars constr =
         constr
      |> List.map (Atom_.vars)
      |> List.fold_left Set.union Set.empty
        
    let to_string constr = String.concat " && " (List.map Atom_.to_string constr)
        

    let rename constr varmapping = List.map (fun atom -> Atom_.rename atom varmapping) constr

    let models constr valuation = 
      List.for_all (fun atom -> Atom_.models atom valuation) constr
        
    let fold ~const ~var ~neg ~plus ~times ~pow ~le ~correct ~conj =
      List.fold_left (fun c atom -> conj c (Atom_.fold ~const ~var ~neg ~plus ~times ~pow ~le atom)) correct

      
    let drop_nonlinear constr =
      List.filter Atom_.is_linear constr 

    (**returns a list of the coefficients of a variable in all the left sides of the constraints*)
    let get_coefficient_vector var constr = 
        let mon = P.Monomial_.lift var 1 in 
            List.map (fun atom ->P.coeff mon (Atom_.normalised_lhs atom)) constr
  end
