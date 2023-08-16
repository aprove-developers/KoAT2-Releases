open OurBase
open PolyTypes

module PolynomialOverIndeterminate (I : PolyTypes.Indeterminate) (Value : PolyTypes.Ring) = struct
  module Monomial_ = Monomials.MakeOverIndeterminate (I) (Value)
  module ScaledMonomial_ = ScaledMonomials.MakeOverIndeterminate (I) (Value)
  module Valuation_ = Valuation.MakeOverIndeterminate (I) (Value)

  type valuation = Valuation_.t
  type monomial = Monomial_.t
  type scaled_monomial = ScaledMonomial_.t
  type value = Value.t
  type indeterminate = I.t

  (* Maybe instead we just use a map here? *)
  module MonMap = MakeMapCreators1 (Monomial_)

  type t = (monomial, value, Monomial_.comparator_witness) Map.t

  let equal = Map.equal Value.equal
  let compare t1 t2 = Map.compare_direct Value.compare t1 t2
  let make l = List.map ~f:(fun sm -> ScaledMonomial_.(monomial sm, coeff sm)) l |> MonMap.of_alist_exn

  let scaled_monomials (t : t) =
    Map.to_alist ~key_order:`Increasing t |> List.map ~f:(fun (mon, coeff) -> ScaledMonomial_.make coeff mon)


  let delete_monomial mon poly = Map.remove poly mon

  let mult_with_const const poly =
    if Value.equal const Value.zero then
      MonMap.empty
    else
      Map.map poly ~f:(Value.mul const)


  let separate_by_sign poly = Map.partition_tf ~f:(fun value -> Value.(value >= zero)) poly

  let add poly1 poly2 =
    let combine_sms ~key = function
      | `Left a -> Some a
      | `Right b -> Some b
      | `Both (a, b) ->
          let s = Value.add a b in
          if Value.(equal s zero) then
            None
          else
            Some s
    in
    Map.merge poly1 poly2 ~f:combine_sms


  (* Map.merge poly1 poly2 ~combine:(fun ~key -> Value.add) *)

  let mul poly1 poly2 =
    if Map.is_empty poly1 || Map.is_empty poly2 then
      MonMap.empty
    else
      Sequence.cartesian_product (Map.to_sequence poly1) (Map.to_sequence poly2)
      |> Sequence.fold ~init:MonMap.empty ~f:(fun t ((mon1, coeff1), (mon2, coeff2)) ->
             add t (MonMap.singleton (Monomial_.mul mon1 mon2) (Value.mul coeff1 coeff2)))


  let pull_out_common_addends t1 t2 =
    let value_abs v =
      if Value.(compare v zero >= 0) then
        v
      else
        Value.neg v
    in
    let combine_monomials (pulled_out, add_left, add_right) =
      let open Sequence.Merge_with_duplicates_element in
      function
      | Sequence.Merge_with_duplicates_element.Left sm -> (pulled_out, sm :: add_left, add_right)
      | Sequence.Merge_with_duplicates_element.Right sm -> (pulled_out, add_left, sm :: add_right)
      | Sequence.Merge_with_duplicates_element.Both (sml, smr) ->
          let coeffl, coeffr = ScaledMonomial_.(coeff sml, coeff smr) in
          let absl, absr = (value_abs coeffl, value_abs coeffr) in

          if Value.(compare coeffl zero <> compare coeffr zero) then
            (pulled_out, sml :: add_left, smr :: add_right)
          else if Value.compare absl absr > 0 then
            (* |sml| > |smr| => pull out smr *)
            let coeff' = Value.add coeffl (Value.neg coeffr) in
            ( smr :: pulled_out,
              ScaledMonomial_.make coeff' (ScaledMonomial_.monomial sml) :: add_left,
              add_right )
          else if Value.compare absl absr < 0 then
            (* |sml| < |smr| => pull out smr *)
            let coeff' = Value.add coeffr (Value.neg coeffl) in
            ( sml :: pulled_out,
              add_left,
              ScaledMonomial_.make coeff' (ScaledMonomial_.monomial smr) :: add_right )
          else
            (* sml = smr *)
            (sml :: pulled_out, add_left, add_right)
    in

    let poly_to_seq t =
      Map.to_sequence ~order:`Increasing_key t
      |> Sequence.map ~f:(fun (mon, coeff) -> ScaledMonomial_.make coeff mon)
    in

    let pulled_out, add_left, add_right =
      Sequence.merge_with_duplicates
        ~compare:(fun sm1 sm2 ->
          Monomial_.compare (ScaledMonomial_.monomial sm1) (ScaledMonomial_.monomial sm2))
        (poly_to_seq t1) (poly_to_seq t2)
      |> Sequence.fold ~init:([], [], []) ~f:(fun a b -> combine_monomials a b)
    in

    (make pulled_out, (make add_left, make add_right))


  let of_coeff_and_mon_list = make % List.map ~f:(uncurry ScaledMonomial_.make)
  let is_integral = List.for_all ~f:ScaledMonomial_.is_integral % scaled_monomials
  let lift coeff mon = make [ ScaledMonomial_.make coeff mon ]

  let fold ~const ~indeterminate ~neg ~plus ~times ~pow t =
    List.fold_left
      ~f:(fun b scaled -> plus b (ScaledMonomial_.fold ~const ~indeterminate ~times ~pow scaled))
      ~init:(const Value.zero) (scaled_monomials t)


  let degree poly =
    scaled_monomials poly |> List.map ~f:ScaledMonomial_.degree |> List.max_elt ~compare:Int.compare
    |> Option.value ~default:0


  let indeterminate_only_linear i p =
    Sequence.of_list (scaled_monomials p)
    |> Sequence.map ~f:ScaledMonomial_.monomial
    |> Sequence.map ~f:(Monomial_.degree_variable i)
    |> Sequence.for_all ~f:(( >= ) 1)


  let var_only_linear var t =
    match scaled_monomials t with
    | [] -> true
    | p ->
        List.for_all ~f:(( >= ) 1)
          (List.map ~f:ScaledMonomial_.degree
             (List.filter ~f:(fun s -> Set.mem (ScaledMonomial_.vars s) var) p))


  let coeff mon poly =
    scaled_monomials poly
    |> List.find ~f:(fun scaled -> Monomial_.( =~= ) (ScaledMonomial_.monomial scaled) mon)
    |> Option.value_map ~default:Value.zero ~f:ScaledMonomial_.coeff


  let coeffs = List.map ~f:ScaledMonomial_.coeff % scaled_monomials

  let coeff_of_indeterminate var poly =
    let mon = Monomial_.lift var 1 in
    coeff mon poly


  let coeff_of_var var poly = coeff_of_indeterminate (I.of_var var) poly

  let to_string_simplified ?(to_file = false) ?(pretty = false) poly =
    let positive, negative =
      scaled_monomials poly
      |> List.partition_tf ~f:(fun s -> Value.compare (ScaledMonomial_.coeff s) Value.zero > 0)
    in
    let str_list str scaled_monomials =
      match
        scaled_monomials
        |> List.map ~f:(ScaledMonomial_.to_string ~to_file ~pretty)
        |> List.filter ~f:(not % String.equal "")
      with
      | [] -> ""
      | xs -> String.concat ~sep:str xs
    in
    let pos_str = str_list "+" positive in
    let neg_str =
      str_list "-" (List.map ~f:(ScaledMonomial_.mult_with_const (Value.neg Value.one)) negative)
    in
    if String.equal (pos_str ^ neg_str) "" then
      "0"
    else if String.equal neg_str "" then
      pos_str
    else
      pos_str ^ "-" ^ neg_str


  let to_string poly = to_string_simplified poly
  let to_string_pretty poly = to_string_simplified ~pretty:true poly
  let to_string_to_file poly = to_string_simplified ~to_file:true poly

  let monomials poly =
    scaled_monomials poly |> List.map ~f:ScaledMonomial_.monomial
    |> List.filter ~f:(not % Monomial_.equal Monomial_.one)


  let monomials_with_coeffs poly =
    scaled_monomials poly
    |> List.map ~f:(fun mon -> (ScaledMonomial_.coeff mon, ScaledMonomial_.monomial mon))


  let of_monomial mon = lift Value.one mon
  let of_power var n = of_monomial (Monomial_.lift var n)

  let of_constant c =
    if Value.equal c Value.zero then
      make []
    else
      lift c Monomial_.one


  let of_indeterminate i = of_power i 1
  let of_var = of_indeterminate % I.of_var

  let of_coeff_list coeffs vars =
    match List.zip coeffs vars with
    | List.Or_unequal_lengths.Unequal_lengths -> make []
    | List.Or_unequal_lengths.Ok zipped ->
        List.map ~f:(fun (c, v) -> ScaledMonomial_.make c (Monomial_.of_indeterminate v)) zipped |> make


  let var str = of_var (Var.of_string str)
  let value c = of_constant (Value.of_int c)
  let real_helper n = of_var (Var.mk_helper Var.Real n)
  let int_helper n = of_var (Var.mk_helper Var.Int n)
  let of_int = value

  (* Gets the constant *)
  let get_constant poly = coeff Monomial_.one poly

  let get_indeterminate t =
    match scaled_monomials t with
    | [ scaled ] when Value.equal Value.one (ScaledMonomial_.coeff scaled) -> (
        let pow_list = Sequence.to_list @@ Monomial_.to_sequence @@ ScaledMonomial_.monomial scaled in
        match pow_list with
        | [ (ind, 1) ] -> Some ind
        | _ -> None)
    | _ -> None


  let indeterminates poly =
    monomials poly |> List.map ~f:Monomial_.indeterminates |> List.join |> Set.stable_dedup_list (module I)


  let vars poly = poly |> monomials |> List.map ~f:Monomial_.vars |> VarSet.union_list

  let is_indeterminate poly =
    poly |> monomials |> fun monomials ->
    List.length monomials == 1
    && Monomial_.is_univariate_linear (List.hd_exn monomials)
    && Value.( =~= ) (coeff (List.hd_exn monomials) poly) Value.one


  let is_indeterminate_plus_constant poly = poly |> delete_monomial Monomial_.one |> is_indeterminate

  let is_sum_of_indeterminates_plus_constant poly =
    poly |> delete_monomial Monomial_.one |> scaled_monomials
    |> List.for_all ~f:(fun scaled ->
           Value.( =~= ) (ScaledMonomial_.coeff scaled) Value.one
           && Monomial_.is_univariate_linear (ScaledMonomial_.monomial scaled))


  let is_univariate_linear poly = degree poly <= 1 && Set.length (vars poly) <= 1
  let is_const poly = degree poly <= 0

  let no_constant_addend poly =
    not @@ (is_const poly || List.exists ~f:(fun sm -> ScaledMonomial_.degree sm = 0) (scaled_monomials poly))


  let is_linear poly = degree poly <= 1

  let rename varmapping poly =
    scaled_monomials poly |> List.map ~f:(ScaledMonomial_.rename varmapping) |> make


  let degree_coeff_list (poly : t) =
    if Set.length (vars poly) <= 1 then
      let tuples_deg_coeff =
        List.map ~f:(fun s -> (ScaledMonomial_.degree s, ScaledMonomial_.coeff s)) (scaled_monomials poly)
      in
      let missing_degrees =
        Set.diff
          (Set.of_list (module Int) (List.range ~stride:1 ~start:`inclusive ~stop:`inclusive 0 (degree poly)))
          (Set.of_list (module Int) (List.map ~f:Tuple2.first tuples_deg_coeff))
      in
      missing_degrees |> Set.to_list
      |> List.fold_left ~f:(fun tuples i -> (i, Value.zero) :: tuples) ~init:tuples_deg_coeff
      |> List.sort ~compare:(fun t1 t2 -> Tuple2.first t1 - Tuple2.first t2)
      |> List.map ~f:Tuple2.second
    else
      []


  type outer_t = t

  module BaseMathImpl : PolyTypes.BaseMath with type t = outer_t = struct
    type t = outer_t

    let zero = make []
    let one = lift Value.one Monomial_.one
    let neg poly = mult_with_const (Value.neg Value.one) poly
    let add = add
    let mul = mul
    let pow poly d = Util.iterate_n_times (mul poly) d one
  end

  include PolyTypes.MakeMath (BaseMathImpl)

  module BasePartialOrderImpl : PolyTypes.BasePartialOrder with type t = outer_t = struct
    type t = outer_t

    let ( =~= ) poly1 poly2 = equal poly1 poly2

    let ( > ) p1 p2 =
      match (scaled_monomials p1, scaled_monomials p2) with
      (* TODO Find some rules to compare polynomials *)
      | [ s1 ], [ s2 ] -> ScaledMonomial_.( > ) s1 s2
      | _ -> None
  end

  include PolyTypes.MakePartialOrder (BasePartialOrderImpl)

  let is_zero poly = poly =~= zero
  let is_one poly = poly =~= one

  let instantiate (substitution : Value.t -> t) =
    fold ~const:substitution ~indeterminate:of_indeterminate ~neg ~plus:add ~times:mul ~pow


  let eval_f poly f =
    scaled_monomials poly
    |> List.map ~f:(fun scaled -> ScaledMonomial_.eval_f scaled f)
    |> List.fold_left ~f:Value.add ~init:Value.zero


  let eval poly valuation =
    scaled_monomials poly
    |> List.map ~f:(fun scaled -> ScaledMonomial_.eval scaled valuation)
    |> List.fold_left ~f:Value.add ~init:Value.zero


  let substitute_f substitution =
    fold ~const:of_constant ~indeterminate:substitution ~neg ~plus:add ~times:mul ~pow


  let substitute ind ~replacement =
    substitute_f (fun target_ind ->
        if I.equal ind target_ind then
          replacement
        else
          of_indeterminate target_ind)


  let eval_partial poly valuation =
    substitute_f
      (fun ind -> Option.map ~f:of_constant (Valuation_.eval_opt ind valuation) |? of_indeterminate ind)
      poly
end

module PolynomialOver (Value : PolyTypes.Ring) = struct
  include PolynomialOverIndeterminate (VarIndeterminate) (Value)

  let substitute_all substitution t =
    substitute_f (fun var -> Option.value ~default:(of_var var) @@ Map.find substitution var) t
end

module Polynomial = struct
  include PolynomialOver (OurInt)

  let max_of_occurring_constants =
    fold ~const:OurInt.abs
      ~indeterminate:(fun _ -> OurInt.one)
      ~neg:identity ~plus:OurInt.add ~times:OurInt.mul ~pow:OurInt.pow
end

module RealPolynomial = struct
  include PolynomialOver (OurFloat)

  let max_of_occurring_constants =
    fold ~const:OurFloat.abs
      ~indeterminate:(fun _ -> OurFloat.one)
      ~neg:identity ~plus:OurFloat.add ~times:OurFloat.mul ~pow:OurFloat.pow


  let of_intpoly =
    Polynomial.fold ~const:(of_constant % OurFloat.of_ourint) ~indeterminate:of_var ~neg ~plus:add ~times:mul
      ~pow


  let of_intconstant = of_constant % OurFloat.of_ourint
end

module RationalPolynomial = struct
  include PolynomialOver (OurRational)

  let normalize poly =
    let coeff_inv =
      coeffs poly
      |> List.filter ~f:(not % OurRational.is_integer)
      |> List.map ~f:Tuple2.second
      |> List.fold ~f:OurInt.lcm ~init:OurInt.one
      |> OurInt.abs |> OurRational.of_ourint
    in
    poly |> mult_with_const coeff_inv
    |> fold
         ~const:(Polynomial.of_constant % OurRational.to_ourint)
         ~indeterminate:Polynomial.of_var ~neg:Polynomial.neg ~plus:Polynomial.add ~times:Polynomial.mul
         ~pow:Polynomial.pow


  let normalize_return_factor poly =
    let coeff_inv =
      coeffs poly
      |> List.filter ~f:(not % OurRational.is_integer)
      |> List.map ~f:Tuple2.second
      |> List.fold ~f:OurInt.lcm ~init:OurInt.one
      |> OurInt.abs |> OurRational.of_ourint
    in
    ( poly |> mult_with_const coeff_inv
      |> fold
           ~const:(Polynomial.of_constant % OurRational.to_ourint)
           ~indeterminate:Polynomial.of_var ~neg:Polynomial.neg ~plus:Polynomial.add ~times:Polynomial.mul
           ~pow:Polynomial.pow,
      coeff_inv )


  let overapprox =
    fold
      ~const:(Polynomial.of_constant % OurRational.ceil % OurRational.abs)
      ~indeterminate:Polynomial.of_var ~neg:Polynomial.neg ~plus:Polynomial.add ~times:Polynomial.mul
      ~pow:Polynomial.pow


  let of_intpoly =
    Polynomial.fold
      ~const:(of_constant % OurRational.of_ourint)
      ~indeterminate:of_var ~neg ~plus:add ~times:mul ~pow


  let is_integer_poly poly = List.for_all ~f:OurRational.is_integer @@ coeffs poly
end

module ParameterPolynomialOver (Value : PolyTypes.Ring) = struct
  module Outer = PolynomialOver (PolynomialOver (Value))
  module Inner = PolynomialOver (Value)
  include Outer

  let eval_coefficients f =
    Outer.fold
      ~const:(fun inner -> Inner.of_constant (Inner.eval_f inner f))
      ~indeterminate:Inner.of_var ~neg:Inner.neg ~plus:Inner.add ~times:Inner.mul ~pow:Inner.pow


  (* Transforms the template polynomial such that all inner values get lifted to the outer polynomial. *)
  (* Example: (2a+b)x + (3a)y - 1 gets transformed to 2ax + bx + 3ay - 1 *)
  let flatten (templatepoly : Outer.t) : Inner.t =
    Outer.fold ~const:identity ~indeterminate:Inner.of_var ~neg:Inner.neg ~plus:Inner.add ~times:Inner.mul
      ~pow:Inner.pow templatepoly


  (* Lifts a polynomial to a parameter polynomial such that the inner structure is kept.*)
  (* Example: 2x +3 is interpreted as 2x+3 and not as the constant polynomial (2x+3)*(1)*)
  let of_polynomial (poly : Inner.t) : t =
    Inner.fold
      ~const:(fun value -> of_constant (Inner.of_constant value))
      ~indeterminate:of_var ~neg ~plus:add ~times:mul ~pow poly
end

module ParameterPolynomial = ParameterPolynomialOver (OurInt)
module RealParameterPolynomial = ParameterPolynomialOver (OurFloat)
