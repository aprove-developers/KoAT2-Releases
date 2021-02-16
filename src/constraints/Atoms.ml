open Batteries
open Polynomials
open BoundsInst
   
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
    type compkind = LE | LT [@@deriving eq, ord]
     
    type t = P.t * compkind [@@deriving eq, ord]

    let comp_to_string =
      function
      | LE -> "<="
      | LT -> "<"

    (* Helper function *)
    let normalise poly1 poly2 = function
      | Comparator.LE -> (P.sub poly1 poly2, LE)
      | Comparator.GE -> (P.sub poly2 poly1, LE)
      | Comparator.LT -> (P.sub poly1 poly2, LT)
      | Comparator.GT -> (P.sub poly2 poly1, LT)

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
    let (=~=) (poly1,comp1) (poly2,comp2) = P.(poly1 =~= poly2) && equal_compkind comp1 comp2

    let neg (poly,comp) = (P.neg poly, comp)
              
    let to_string ?(to_file=false) (poly,comp) = (P.to_string poly) ^ (comp_to_string comp) ^ "0"
        
    let vars (poly,_) = P.vars poly

    let normalised_lhs (poly,_) = poly
             
    let rename (poly,comp) varmapping = (P.rename varmapping poly, comp)

    let fold ~subject ~le ~lt (poly,comp) =
      if comp = LE then
        le (subject poly) (subject P.zero)
      else
        lt (subject poly) (subject P.zero)
      
                               (*
    (* TODO It's maybe possible to compare polynomials without full evaluation *)
    (* However, there are probably more expensive operations *)
    let models atom valuation =
      P.Value.Compare.((P.eval atom valuation) <= P.Value.zero)
                                *)
    let is_linear (poly,comp) = P.is_linear poly 
        
    let get_coefficient var atom =
      P.coeff_of_var var (normalised_lhs atom)
      
    let get_constant (poly,comp) =
      P.get_constant (P.neg poly)
end

module Atom =
  struct
    include AtomOver(Polynomial)

    let to_string ?(to_file=false) (poly,comp) =
      Polynomial.separate_by_sign poly
      |> (fun (positive, negative) -> (if to_file then (Polynomial.to_string_to_file positive) 
                                       else (Polynomial.to_string positive)) ^ (comp_to_string comp) ^ 
                                       (if to_file then (Polynomial.to_string_to_file (Polynomial.neg negative)) 
                                       else (Polynomial.to_string (Polynomial.neg negative))))
      
    let max_of_occurring_constants (poly,_) =
      Polynomial.max_of_occurring_constants poly

    let remove_strict (poly, comp) = 
      match comp with 
      | LE -> (poly, comp)
      | LT -> (Polynomial.add poly Polynomial.one, LE)

  end

module ParameterAtom =
  struct
    include AtomOver(ParameterPolynomial)

    let remove_strict (poly, comp) = 
      match comp with 
      | LE -> (poly, comp)
      | LT -> (ParameterPolynomial.add poly ParameterPolynomial.one, LE)
  end

module BoundAtom =
  struct
    include AtomOver(Bound)
  end

module RealAtom =
  struct
    include AtomOver(RealPolynomial)

    let to_string ?(to_file=false) (poly,comp) =
      RealPolynomial.separate_by_sign poly
      |> (fun (positive, negative) -> RealPolynomial.to_string positive ^ (comp_to_string comp) ^ RealPolynomial.to_string (RealPolynomial.neg negative))

    let max_of_occurring_constants (poly,_) =
      RealPolynomial.max_of_occurring_constants poly

    let of_intatom ((poly,comp):Atom.t): t =
      let realpoly_minus_1 = RealPolynomial.sub RealPolynomial.zero RealPolynomial.one in
      match comp with
      (* This is necessary because for example x<0 is equivalent to x<-1 in the deterministic case*)
      | LE -> mk Comparator.LE (RealPolynomial.of_intpoly poly) RealPolynomial.zero
      | LT -> mk Comparator.LE (RealPolynomial.of_intpoly poly) realpoly_minus_1

  end

module RealParameterAtom =
  struct
    include AtomOver(RealParameterPolynomial)

    let of_int_para_atom atom =
      mk Comparator.LE (RealParameterPolynomial.of_int_parapoly atom) RealParameterPolynomial.zero

    let get_constant (poly,_) =
      P.get_constant (P.neg poly)
  end