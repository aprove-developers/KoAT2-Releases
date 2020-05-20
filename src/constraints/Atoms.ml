open Batteries
open Polynomials
   
module AtomOver(P : ConstraintTypes.Atomizable) =
(*Polynomial Constraints of the form p1<p2, p1<=p2, etc. Conjunctions of these constraints form the real constraints*)
struct
    module P = P

    type polynomial = P.t
    type value = P.value

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
    type t = P.t [@@deriving eq, ord]


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

    module Infix = struct
      let (>) = mk_gt
      let (>=) = mk_ge 
      let (<) = mk_lt 
      let (<=) = mk_le      
    end

    (* TODO We can not decide all equalities right now because of some integer arithmetic *)
    (* Maybe use SMT-solver here *)
    let (=~=) = P.(=~=)

    let neg = P.neg
              
    let to_string ?(to_file=false) ?(comp=" <= ") atom = (P.to_string atom) ^ comp ^ "0"
        
    let vars = P.vars

    let normalised_lhs atom = atom
             
    let rename atom varmapping = P.rename varmapping atom

    let fold ~subject ~le poly =
      le (subject poly) (subject P.zero)
      
                               (*
    (* TODO It's maybe possible to compare polynomials without full evaluation *)
    (* However, there are probably more expensive operations *)
    let models atom valuation =
      P.Value.Compare.((P.eval atom valuation) <= P.Value.zero)
                                *)
    let is_linear = P.is_linear 
        
    let get_coefficient var atom =
      P.coeff_of_var var (normalised_lhs atom)
      
    let get_constant atom =
      P.get_constant (P.neg atom)
end

module Atom =
  struct
    include AtomOver(Polynomial)

    let to_string ?(to_file=false) ?(comp=" <= ") atom =
      Polynomial.separate_by_sign atom
      |> (fun (positive, negative) -> (if to_file then (Polynomial.to_string_to_file positive) 
                                       else (Polynomial.to_string positive)) ^ comp ^ 
                                       (if to_file then (Polynomial.to_string_to_file (Polynomial.neg negative)) 
                                       else (Polynomial.to_string (Polynomial.neg negative))))
      
    let max_of_occurring_constants =
      Polynomial.max_of_occurring_constants

  end

module ParameterAtom =
  struct
    include AtomOver(ParameterPolynomial)
  end

module BoundAtom =
  struct
    include AtomOver(Bound)
  end