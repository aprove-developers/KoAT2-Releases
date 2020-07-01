open Batteries
open BoundsInst
open Polynomials

module AtomOver(P : ConstraintTypes.Atomizable) =
(*Polynomial Constraints of the form p1<p2, p1<=p2, etc. Conjunctions of these constraints form the constraints*)
(* Internally an atom consists of a polynomial p and a compkind. It corresponds then to p compkind 0 where compkind is either < or <=*)
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

    (* Always in normalised form: polynomial <= 0 / polynomial < 0*)
    type compkind = LE | LT [@@deriving eq, ord]

    type t = P.t * compkind [@@deriving eq, ord]


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

    let neg (poly,comp) = (P.neg poly,comp)

    let comp_to_string =
      function
      | LE -> "<="
      | LT -> "<"

    let to_string ?(compfunc=comp_to_string) (poly,comp) = (P.to_string poly) ^ (compfunc comp) ^ "0"

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
    let is_linear (poly,_) = P.is_linear poly

    let get_coefficient var atom =
      P.coeff_of_var var (normalised_lhs atom)

end

module Atom =
  struct
    include AtomOver(Polynomial)

    let to_string ?(compfunc=comp_to_string) (poly,comp) =
      Polynomial.separate_by_sign poly
      |> (fun (positive, negative) -> Polynomial.to_string positive ^ (compfunc comp) ^ Polynomial.to_string (Polynomial.neg negative))

    let max_of_occurring_constants (poly,_) =
      Polynomial.max_of_occurring_constants poly

    (* Specialized Equality for integer Atoms *)
    let rec (=~=) (poly1,comp1) (poly2,comp2) = match (comp1,comp2) with
      | (LE,LT) -> (Polynomial.(poly1-one), LT) =~= (poly2,LT)
      | (LT,LE) -> (poly1,LT) =~= (Polynomial.(poly2 - one),LT)
      | (LT,LT) -> Polynomial.(poly1 =~= poly2)
      | (LE,LE) -> Polynomial.(poly1 =~= poly2)

    let get_constant (poly, compkind) =
      match compkind with
      | LE -> P.get_constant (P.neg poly)
      (* Implicitly convert from < to <=*)
      | LT -> (P.get_constant (P.neg @@ P.add P.one poly))
  end

module ParameterAtom =
  struct
    include AtomOver(ParameterPolynomial)

    let get_constant (poly,_) =
      P.get_constant (P.neg poly)
  end

module BoundAtom =
  struct
    include AtomOver(Bound)
  end

module RealAtom =
  struct
    include AtomOver(RealPolynomial)

    let to_string ?(compfunc=comp_to_string) (poly,comp) =
      RealPolynomial.separate_by_sign poly
      |> (fun (positive, negative) -> RealPolynomial.to_string positive ^ (compfunc comp) ^ RealPolynomial.to_string (RealPolynomial.neg negative))

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
