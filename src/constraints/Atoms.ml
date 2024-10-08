open! OurBase
open Polynomials

(** This module functor can be used to create generic atoms.
    However, some specific required functions need to be filled in after instantiating this functor. *)
module GenericAtomHelperOver (P : PolyTypes.Polynomial) = struct
  module Inner = struct
    type monomial = P.monomial
    type polynomial = P.t
    type value = P.value
    type compkind = LE | LT [@@deriving eq, ord]

    type t = P.t * compkind [@@deriving eq, ord]
    (** Always in normalised form: polynomial comparator 0 *)

    let sexp_of_t = Sexplib0.Sexp_conv.sexp_of_opaque

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


    let mk_gt poly1 poly2 = (P.sub poly2 poly1, LT)
    let mk_ge poly1 poly2 = (P.sub poly2 poly1, LE)
    let mk_lt poly1 poly2 = (P.sub poly1 poly2, LT)
    let mk_le poly1 poly2 = (P.sub poly1 poly2, LE)

    module Infix = struct
      let ( > ) = mk_gt
      let ( >= ) = mk_ge
      let ( < ) = mk_lt
      let ( <= ) = mk_le
    end

    let neg = function
      | poly, LE -> mk_gt poly P.zero
      | poly, LT -> mk_ge poly P.zero


    let flip_comp = function
      | poly, LE -> mk_ge poly P.zero
      | poly, LT -> mk_gt poly P.zero


    let to_string ?(to_file = false) ?(pretty = false) (poly, comp) =
      P.to_string poly ^ comp_to_string comp ^ "0"


    let vars (poly, _) = P.vars poly
    let non_constant_monomials (poly, _) = P.non_constant_monomials poly
    let rename (poly, comp) varmapping = (P.rename varmapping poly, comp)

    let fold ~subject ~le ~lt (poly, comp) =
      if equal_compkind comp LE then
        le (subject poly) (subject P.zero)
      else
        lt (subject poly) (subject P.zero)


    let map_var ~subject =
      Tuple2.map1 (P.fold ~const:P.of_constant ~indeterminate:subject ~plus:P.add ~times:P.mul ~pow:P.pow)


    let is_linear (poly, comp) = P.is_linear poly

    let is_lt (_, comp) =
      match comp with
      | LT -> true
      | _ -> false


    let is_le (_, comp) =
      match comp with
      | LE -> true
      | _ -> false
  end

  include Inner
  include Base.Comparator.Make (Inner)
end

(** Specialised Atoms over Integers. *)
module Atom = struct
  module Inner = struct
    type polynomial = Polynomial.t
    type value = Polynomial.value
    type monomial = Polynomial.monomial

    type t = Polynomial.t [@@deriving eq, ord]
    (** Invariant: Always in cannonical form p ≤ 0, where the content of p is 1 *)

    let poly = identity
    let poly_lt t = Polynomial.(t - one)
    let sexp_of_t = Sexplib0.Sexp_conv.sexp_of_opaque

    let cannonical poly =
      let content, primitive_part = Polynomial.primitive_part_content_factorisation poly in
      primitive_part


    let mk_gt poly1 poly2 = cannonical Polynomial.(poly2 - poly1 + one)
    let mk_ge poly1 poly2 = cannonical Polynomial.(poly2 - poly1)
    let mk_lt poly1 poly2 = cannonical Polynomial.(poly1 - poly2 + one)
    let mk_le poly1 poly2 = cannonical Polynomial.(poly1 - poly2)

    module Infix = struct
      let ( > ) = mk_gt
      let ( >= ) = mk_ge
      let ( < ) = mk_lt
      let ( <= ) = mk_le
    end

    (* Nothing to do. All Atoms represent non-strict inequalities *)
    let remove_strict t = t
    let flip_comp t = mk_ge t Polynomial.zero
    let neg t = mk_gt t Polynomial.zero
    let vars = Polynomial.vars
    let non_constant_monomials = Polynomial.non_constant_monomials % poly
    let rename = flip Polynomial.rename
    let is_linear = Polynomial.is_linear
    let fold ~subject ~le ~lt t = le (subject t) (subject Polynomial.zero)

    let map_var ~subject =
      Polynomial.fold ~const:Polynomial.of_constant ~indeterminate:subject ~plus:Polynomial.add
        ~times:Polynomial.mul ~pow:Polynomial.pow


    let is_le _ = true
    let is_lt _ = false
    let get_constant = OurInt.neg % Polynomial.get_constant
    let get_coefficient mon = Polynomial.coeff mon

    let comp_to_string ~pretty =
      if pretty then
        " ≤ "
      else
        "<="


    let to_string ?(to_file = false) ?(pretty = false) poly =
      let poly_print =
        if to_file then
          Polynomial.to_string_to_file
        else if pretty then
          Polynomial.to_string_pretty
        else
          Polynomial.to_string
      in
      Polynomial.separate_by_sign poly |> fun (positive, negative) ->
      poly_print positive ^ comp_to_string ~pretty ^ poly_print (Polynomial.neg negative)


    let max_of_occurring_constants poly = Polynomial.max_of_occurring_constants poly
    let ( =~= ) = Polynomial.equal
  end

  include Inner
  include Comparator.Make (Inner)
end

module RationalAtom = struct
  include GenericAtomHelperOver (PolynomialOver (OurRational))

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

  exception StrictUnremovable of string

  let remove_strict = function
    | p, LE -> (p, LE)
    | p, LT -> raise (StrictUnremovable ("Can not remove strict comperator in " ^ to_string (p, LT)))


  let of_intatom (poly : Atom.t) : t = mk_le (RationalPolynomial.of_intpoly poly) RationalPolynomial.zero

  let get_constant t =
    let poly, _ = remove_strict t in
    OurRational.neg (RationalPolynomial.get_constant poly)


  let get_coefficient mon t =
    let poly, _ = remove_strict t in
    RationalPolynomial.coeff mon poly


  let ( =~= ) = equal
end

module ParameterAtomOver (Value : PolyTypes.Ring) = GenericAtomHelperOver (ParameterPolynomialOver (Value))

module ParameterAtom = struct
  include ParameterAtomOver (OurInt)

  let remove_strict (poly, comp) =
    match comp with
    | LE -> (poly, comp)
    | LT -> (ParameterPolynomial.add poly ParameterPolynomial.one, LE)


  let get_constant t =
    let poly, _ = remove_strict t in
    Polynomial.neg (ParameterPolynomial.get_constant poly)


  let get_coefficient mon t =
    let poly, _ = remove_strict t in
    ParameterPolynomial.coeff mon poly


  let ( =~= ) = equal
end

module RationalParameterAtom = struct
  include ParameterAtomOver (OurRational)
  module Mon = Monomials.Make (PolynomialOver (OurRational))

  let replace_nonlinear_monomials_with_temp_vars ((p, c) : t) =
    let p =
      Sequence.of_list (RationalParameterPolynomial.monomials_with_coeffs p)
      |> Sequence.map ~f:(fun (c, m) ->
             if Mon.is_constant m || Mon.is_univariate_linear m then
               RationalParameterPolynomial.of_coeff_and_mon_list [ (c, m) ]
             else
               let m = Mon.of_var (Var.fresh_id Var.Real ()) in
               RationalParameterPolynomial.of_coeff_and_mon_list [ (c, m) ])
      |> RationalParameterPolynomial.sum
    in
    (p, c)


  exception StrictUnremovable of string

  let remove_strict = function
    | p, LE -> (p, LE)
    | p, LT -> raise (StrictUnremovable ("Can not remove strict comperator in " ^ to_string (p, LT)))


  let get_constant t =
    let poly, _ = remove_strict t in
    RationalPolynomial.neg (RationalParameterPolynomial.get_constant poly)


  let get_coefficient mon t =
    let poly, _ = remove_strict t in
    RationalParameterPolynomial.coeff mon poly


  let ( =~= ) = equal
end
