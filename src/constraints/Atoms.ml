open OurBase
open Polynomials
open Bounds

module AtomOver (P : ConstraintTypes.Atomizable) =
(*Polynomial Constraints of the form p1<p2, p1<=p2, etc. Conjunctions of these constraints form the real constraints*)
struct
  module P = P

  type polynomial = P.t
  type value = P.value

  module Comparator = struct
    type t = GT | GE | LT | LE

    let values = [ GT; GE; LT; LE ]

    let to_string = function
      | GT -> ">"
      | GE -> ">="
      | LT -> "<"
      | LE -> "<="


    let str_values = List.map ~f:to_string values

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

  let comp_to_string ?(pretty = false) = function
    | LE ->
        if pretty then
          " ≤ "
        else
          "<="
    | LT ->
        if pretty then
          " < "
        else
          "<"


  (* Helper function *)
  let normalise poly1 poly2 = function
    | Comparator.LE -> (P.sub poly1 poly2, LE)
    | Comparator.GE -> (P.sub poly2 poly1, LE)
    | Comparator.LT -> (P.sub poly1 poly2, LT)
    | Comparator.GT -> (P.sub poly2 poly1, LT)


  let mk comp poly1 poly2 = (*P.scale_coefficients*) normalise poly1 poly2 comp
  let mk_gt = mk Comparator.GT
  let mk_ge = mk Comparator.GE
  let mk_lt = mk Comparator.LT
  let mk_le = mk Comparator.LE

  module Infix = struct
    let ( > ) = mk_gt
    let ( >= ) = mk_ge
    let ( < ) = mk_lt
    let ( <= ) = mk_le
  end

  (* TODO We can not decide all equalities right now because of some integer arithmetic *)
  (* Maybe use SMT-solver here *)
  let ( =~= ) (poly1, comp1) (poly2, comp2) = P.(poly1 =~= poly2) && equal_compkind comp1 comp2

  let neg = function
    | poly, LE -> (P.neg poly, LT)
    | poly, LT -> (P.neg poly, LE)


  let flip_comp = function
    | poly, LE -> (P.neg poly, LE)
    | poly, LT -> (P.neg poly, LT)


  let to_string ?(to_file = false) ?(pretty = false) (poly, comp) =
    P.to_string poly ^ comp_to_string comp ^ "0"


  let remove_strict = function
    | p, LE -> (p, LE)
    | p, LT ->
        if P.is_integral p then
          (P.(add one p), LE)
        else
          raise
            (ConstraintTypes.StrictUnremovable ("Can not remove strict comperator in " ^ to_string (p, LT)))


  let vars (poly, _) = P.vars poly
  let normalised_lhs (poly, _) = poly
  let rename (poly, comp) varmapping = (P.rename varmapping poly, comp)

  let fold ~subject ~le ~lt (poly, comp) =
    if equal_compkind comp LE then
      le (subject poly) (subject P.zero)
    else
      lt (subject poly) (subject P.zero)


  (*
    (* TODO It's maybe possible to compare polynomials without full evaluation *)
    (* However, there are probably more expensive operations *)
    let models atom valuation =
      P.Value.Compare.((P.eval atom valuation) <= P.Value.zero)
                                *)
  let is_linear (poly, comp) = P.is_linear poly
  let get_coefficient var atom = P.coeff_of_var var (normalised_lhs atom)

  let get_constant (poly, compkind) =
    match compkind with
    | LE -> P.get_constant (P.neg poly)
    (* Implicitly convert from < to <=*)
    | LT -> P.get_constant (P.neg @@ P.add P.one poly)


  let poly (poly, _) = poly

  let is_lt (_, comp) =
    match comp with
    | LT -> true
    | _ -> false


  let is_le (_, comp) =
    match comp with
    | LE -> true
    | _ -> false
end

module Atom = struct
  include AtomOver (PolynomialOver (OurInt))

  let to_string ?(to_file = false) ?(pretty = false) (poly, comp) =
    let poly_print =
      if to_file then
        Polynomial.to_string_to_file
      else if pretty then
        Polynomial.to_string_pretty
      else
        Polynomial.to_string
    in
    Polynomial.separate_by_sign poly |> fun (positive, negative) ->
    poly_print positive ^ comp_to_string comp ~pretty ^ poly_print (Polynomial.neg negative)


  let max_of_occurring_constants (poly, _) = Polynomial.max_of_occurring_constants poly

  (* Specialized Equality for integer Atoms *)
  let rec ( =~= ) (poly1, comp1) (poly2, comp2) =
    match (comp1, comp2) with
    | LE, LT -> (Polynomial.(poly1 - one), LT) =~= (poly2, LT)
    | LT, LE -> (poly1, LT) =~= (Polynomial.(poly2 - one), LT)
    | LT, LT -> Polynomial.(poly1 =~= poly2)
    | LE, LE -> Polynomial.(poly1 =~= poly2)
end

module RealAtom = struct
  include AtomOver (PolynomialOver (OurRational))

  let to_string ?(to_file = false) ?(pretty = false) (poly, comp) =
    let poly_print =
      if to_file then
        RationalPolynomial.to_string_to_file
      else if pretty then
        RationalPolynomial.to_string_pretty
      else
        RationalPolynomial.to_string
    in
    RationalPolynomial.separate_by_sign poly |> fun (positive, negative) ->
    poly_print positive ^ comp_to_string comp ~pretty ^ poly_print (RationalPolynomial.neg negative)


  let max_of_occurring_constants (poly, _) = RationalPolynomial.max_of_occurring_constants poly

  let of_intatom ((poly, comp) : Atom.t) : t =
    let realpoly_minus_1 = RationalPolynomial.sub RationalPolynomial.zero RationalPolynomial.one in
    match comp with
    (* This is necessary because for example x<0 is equivalent to x<-1 in the deterministic case*)
    | LE -> mk Comparator.LE (RationalPolynomial.of_intpoly poly) RationalPolynomial.zero
    | LT -> mk Comparator.LE (RationalPolynomial.of_intpoly poly) realpoly_minus_1
end

module ParameterAtomOver (Value : PolyTypes.Ring) = AtomOver (ParameterPolynomialOver (Value))

module ParameterAtom = struct
  include ParameterAtomOver (OurInt)

  let remove_strict (poly, comp) =
    match comp with
    | LE -> (poly, comp)
    | LT -> (ParameterPolynomial.add poly ParameterPolynomial.one, LE)
end

module RealParameterAtom = ParameterAtomOver (OurRational)
