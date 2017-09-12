open Batteries
open PolyTypes
open ConstraintTypes

module Make(P : Polynomial) =
(*Polynomial Constraints of the form p1<p2, p1<=p2, etc. Conjunctions of these constraints form the real constraints*)
struct
    module Polynomial_ = P

    module Comparator =
      struct
        type t = GT | GE | LT | LE

        let values = [GT; GE; LT; LE]

        (* Helper function *)
        let is_inverted (comp1 : t) (comp2 : t) =
          match (comp1, comp2) with
          | (GT, LT) -> true
          | (GE, LE) -> true
          | (LT, GT) -> true
          | (LE, GE) -> true
          | (_,_) -> false

        let to_string = function
          | GT -> ">"
          | GE -> ">="
          | LT -> "<"
          | LE -> "<="
               
        let str_values = List.map to_string values

                       (*
        let to_function =
          let open P.Value.Compare in function
          | GT -> (>)
          | GE -> (>=)
          | LT -> (<)
          | LE -> (<=)
                        *)
      end

    (* Always in normalised form: polynomial <= 0 *)
    type t = P.t 


    (* Helper function *)
    let normalise poly1 poly2 = function
      | Comparator.LE -> P.sub poly1 poly2
      | Comparator.GE -> P.sub poly2 poly1
      | Comparator.LT -> P.sub (P.add poly1 P.one) poly2
      | Comparator.GT -> P.sub (P.add poly2 P.one) poly1

    let mk comp poly1 poly2 =
      (*P.scale_coefficients*) (normalise poly1 poly2 comp)

    let mk_gt = mk Comparator.GT
    let mk_ge = mk Comparator.GE
    let mk_lt = mk Comparator.LT
    let mk_le = mk Comparator.LE


    (* TODO We can not decide all equalities right now because of some integer arithmetic *)
    (* Maybe use SMT-solver here *)
    let (=~=) = P.(=~=)
            
    let to_string atom = (P.to_string atom) ^ " <= 0"
        
    let is_linear = P.is_linear 
        
    let vars = P.vars
        
    let normalised_lhs atom = atom
             
    let rename atom varmapping = P.rename varmapping atom

                               (*
    (* TODO It's maybe possible to compare polynomials without full evaluation *)
    (* However, there are probably more expensive operations *)
    let models atom valuation =
      P.Value.Compare.((P.eval atom valuation) <= P.Value.zero)
                                *)

    let fold ~const ~var ~neg ~plus ~times ~pow ~le poly =
      le (P.fold ~const ~var ~neg ~plus ~times ~pow poly) (P.fold ~const ~var ~neg ~plus ~times ~pow P.zero)
      
end
