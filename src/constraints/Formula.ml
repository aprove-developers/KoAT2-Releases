open Batteries
open PolyTypes
open ConstraintTypes
   
module Make(P : Polynomial) =
  struct

    module Polynomial_ = P
    module Constraint_ = Constraints.Make(P)
    module Atom_ = Atoms.Make(P)
    
    type t = Constraint_.t list

    let mk constr =
      [constr]
           
    let fold ~const ~var ~neg ~plus ~times ~pow ~le ~correct ~conj ~wrong ~disj =
      List.fold_left (fun c constr -> disj c (Constraint_.fold ~const ~var ~neg ~plus ~times ~pow ~le ~correct ~conj constr)) wrong
      
    let disj constraints =
      constraints

    let lift atom =
      mk (Constraint_.lift atom)
    
    let mk_true =
      mk Constraint_.mk_true

    let mk_false =
      []

    let mk_eq poly1 poly2 =
      mk (Constraint_.Infix.(poly1 = poly2))

    let mk_gt p1 p2 = mk (Constraint_.mk_gt p1 p2)
    let mk_ge p1 p2 = mk (Constraint_.mk_ge p1 p2)
    let mk_lt p1 p2 = mk (Constraint_.mk_lt p1 p2)
    let mk_le p1 p2 = mk (Constraint_.mk_le p1 p2)
    
    let mk_and formula1 formula2 =
      List.cartesian_product formula1 formula2
      |> List.map (uncurry Constraint_.mk_and)
      
    let mk_or =
      List.append

    let neg =
      fold ~const:Polynomial_.from_constant
           ~var:Polynomial_.from_var
           ~neg:Polynomial_.neg
           ~plus:Polynomial_.add
           ~times:Polynomial_.mul
           ~pow:Polynomial_.pow
           ~le:mk_gt
           ~correct:mk_false
           ~conj:mk_or
           ~wrong:mk_true
           ~disj:mk_and

    let implies formula1 formula2 =
      mk_or (neg formula1) formula2

    module Infix = struct
      let (=) = mk_eq
      let (>) = mk_gt
      let (>=) = mk_ge 
      let (<) = mk_lt 
      let (<=) = mk_le
      let (&&) = mk_and
      let (||) = mk_or
    end

    (* a <= max{b1,...,bn}   <=>   a<=b1 || ... || a<=bn *)
    let mk_le_than_max poly max_list =
      max_list
      |> List.map (fun max -> Infix.(poly <= max))
      |> List.fold_left mk_or mk_false

    let all =
      List.fold_left mk_and mk_true

    let any =
      List.flatten
      
    let vars formula =
         formula
      |> List.map (Constraint_.vars)
      |> List.fold_left Set.union Set.empty
        
    let to_string constr =
      String.concat " || " (List.map Constraint_.to_string constr)
        
    let rename formula varmapping =
      List.map (fun constr -> Constraint_.rename constr varmapping) formula
        
  end
