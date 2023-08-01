open OurBase
open PolyTypes

module PolynomialOverIndeterminate(I: PolyTypes.Indeterminate)(Value : PolyTypes.Ring) =
  struct
    module Monomial_ = Monomials.MakeOverIndeterminate(I)(Value)
    module ScaledMonomial_ = ScaledMonomials.MakeOverIndeterminate(I)(Value)
    module Valuation_ = Valuation.MakeOverIndeterminate(I)(Value)
    type valuation = Valuation_.t

    type monomial = Monomial_.t
    type scaled_monomial = ScaledMonomial_.t
    type t = ScaledMonomial_.t list [@@deriving eq, ord]
    type value = Value.t
    type indeterminate = I.t

    let make = List.map ~f:(fun (coeff, mon) -> ScaledMonomial_.make coeff mon)

    let is_integral = List.for_all ~f:ScaledMonomial_.is_integral

    let lift coeff mon = [ScaledMonomial_.make coeff mon]

    let of_scaled scaled = scaled

    let fold ~const ~indeterminate ~neg ~plus ~times ~pow =
      List.fold_left ~f:(fun b scaled -> plus b (ScaledMonomial_.fold ~const ~indeterminate ~times ~pow scaled)) ~init:(const Value.zero)

    let degree poly =
      Option.value ~default:0 @@ List.max_elt ~compare:Int.compare (List.map ~f:(ScaledMonomial_.degree) poly )

    let indeterminate_only_linear i p =
      Sequence.map ~f:(ScaledMonomial_.monomial) (Sequence.of_list p)
      |> Sequence.map ~f:(Monomial_.degree_variable i)
      |> Sequence.for_all ~f:((>=) 1)

    let var_only_linear var = function
      | [] -> true
      | p -> List.for_all ~f:((>=) 1) (List.map ~f:ScaledMonomial_.degree (List.filter ~f:(fun s -> Set.mem (ScaledMonomial_.vars s) var) p))

    let coeff mon poly =
         poly
      |> List.filter ~f:(fun scaled -> Monomial_.(=~=) (ScaledMonomial_.monomial scaled) mon)
      |> List.map ~f:ScaledMonomial_.coeff
      |> List.fold_left ~f:Value.add ~init:Value.zero

    let coeffs = List.map ~f:ScaledMonomial_.coeff

    let coeff_of_indeterminate var poly =
        let mon = Monomial_.lift var 1 in
            coeff mon poly

    let coeff_of_var var poly = coeff_of_indeterminate (I.of_var var) poly

    let delete_monomial mon poly =
      List.filter ~f:(fun x -> not (Monomial_.(=~=) (ScaledMonomial_.monomial x) mon)) poly

    let simplify poly =
      let poly' =
        List.sort_and_group ~compare:(fun sm1 sm2 -> Monomial_.compare (ScaledMonomial_.monomial sm1) (ScaledMonomial_.monomial sm2)) poly
        |> List.map ~f:(fun sms ->
          let coeffs = List.map ~f:ScaledMonomial_.coeff sms in
          let sum_coeffs = List.fold ~f:Value.add ~init:Value.zero coeffs in
          ScaledMonomial_.make sum_coeffs (ScaledMonomial_.monomial @@ List.hd_exn sms)) in
        List.filter ~f:(not % (Value.equal Value.zero) % ScaledMonomial_.coeff) poly'

    let to_string_simplified ?(to_file=false) ?(pretty=false) poly =
      let positive, negative = List.partition_tf ~f:(fun s ->  Value.compare (ScaledMonomial_.coeff s) Value.zero > 0) poly in
      let str_list str scaled_monomials = match
        scaled_monomials
        |> List.map ~f:(ScaledMonomial_.to_string ~to_file ~pretty)
        |> List.filter ~f:(not % (String.equal "")) with
          | [] -> ""
          | xs -> String.concat ~sep:str xs in
      let pos_str = str_list "+" positive in
      let neg_str = str_list "-" (List.map ~f:(ScaledMonomial_.mult_with_const (Value.neg Value.one)) negative) in
      if String.equal (pos_str ^ neg_str) "" then "0"
      else if String.equal neg_str "" then pos_str
      else pos_str ^ "-" ^ neg_str

    let to_string poly = to_string_simplified (simplify poly)

    let to_string_pretty poly = to_string_simplified ~pretty:true (simplify poly)

    let to_string_to_file poly = to_string_simplified ~to_file:true (simplify poly)

    let monomials poly =
         poly
      |> simplify
      |> List.map ~f:ScaledMonomial_.monomial
      |> List.filter ~f:(not % Monomial_.equal Monomial_.one)

    let scaled_monomials poly = poly |> simplify

    let monomials_with_coeffs poly =
         poly
      |> simplify
      |> List.map ~f:(fun mon -> ScaledMonomial_.coeff mon, ScaledMonomial_.monomial mon)

    let of_monomial mon = lift Value.one mon

    let of_power var n = of_monomial (Monomial_.lift var n)

    let of_constant c = lift c Monomial_.one

    let of_indeterminate i = of_power i 1
    let of_var = of_indeterminate % I.of_var

    let rec of_coeff_list coeffs vars =
        if (List.length coeffs) == (List.length vars) then
            match (coeffs, vars) with
                |([],[])-> []
                |(c::coefftail, v::varstail)-> (ScaledMonomial_.make c (Monomial_.lift v 1)) :: (of_coeff_list coefftail varstail)
                |_ -> []
        else []

    let var str = of_var (Var.of_string str)

    let value c = of_constant (Value.of_int c)

    let real_helper n = of_var (Var.mk_helper Var.Real n)

    let int_helper n = of_var (Var.mk_helper Var.Int n)

    let of_int = value

    (* Gets the constant *)
    let get_constant poly = coeff Monomial_.one (simplify poly)

    let get_indeterminate t =
      if List.length t = 1 then
        let scaled = List.hd_exn t in
        if Value.equal Value.one (ScaledMonomial_.coeff scaled) then
          let pow_list = Sequence.to_list @@ Monomial_.to_sequence @@ ScaledMonomial_.monomial scaled in
          if List.length pow_list = 1 then
            let (ind,pow) = List.hd_exn pow_list in
            if pow = 1 then Some ind
            else None
          else None
        else None
      else None

    let indeterminates poly =
      monomials (simplify poly)
      |> List.map ~f:Monomial_.indeterminates
      |> List.join
      |> Set.stable_dedup_list (module I)

    let vars poly =
         poly
      |> simplify
      |> monomials
      |> List.map ~f:Monomial_.vars
      |> VarSet.union_list

    let is_indeterminate poly =
         poly
      |> simplify
      |> monomials
      |> fun monomials -> List.length monomials == 1 &&
                            Monomial_.is_univariate_linear (List.hd_exn monomials) && (Value.(=~=) (coeff (List.hd_exn monomials) poly) Value.one)

    let is_indeterminate_plus_constant poly =
         poly
      |> delete_monomial Monomial_.one
      |> is_indeterminate

    let is_sum_of_indeterminates_plus_constant poly =
         poly
      |> delete_monomial Monomial_.one
      |> List.for_all ~f:(fun scaled -> Value.(=~=) (ScaledMonomial_.coeff scaled) Value.one &&
                                       Monomial_.is_univariate_linear (ScaledMonomial_.monomial scaled))

    let is_univariate_linear poly =
      degree poly <= 1 && Set.length (vars poly) <= 1

    let is_const poly = degree poly <= 0

    let no_constant_addend poly = not @@ (is_const poly || List.exists ~f:(fun sm -> ScaledMonomial_.degree sm = 0) poly)

    let is_linear poly = (degree poly <= 1)

    let rename varmapping poly =
      List.map ~f:(ScaledMonomial_.rename varmapping) poly

    let mult_with_const const poly =
      List.map ~f:(ScaledMonomial_.mult_with_const const) poly

    let degree_coeff_list (poly:t) =
      if Set.length (vars poly) <= 1 then
      let tuples_deg_coeff = List.map ~f:(fun s -> (ScaledMonomial_.degree s, ScaledMonomial_.coeff s)) poly in
      let missing_degrees =
        Set.diff
          (Set.of_list (module Int) (List.range ~stride:1 ~start:`inclusive ~stop:`inclusive 0 (degree poly)))
          (Set.of_list (module Int) (List.map ~f:Tuple2.first tuples_deg_coeff))
      in
        missing_degrees
        |> Set.to_list
        |> List.fold_left ~f:(fun tuples i -> (i,Value.zero)::tuples) ~init:tuples_deg_coeff
        |> List.sort ~compare:(fun t1 t2 -> (Tuple2.first t1) - (Tuple2.first t2))
        |> List.map ~f:Tuple2.second
      else []

    type outer_t = t
    module BaseMathImpl : (PolyTypes.BaseMath with type t = outer_t) =
      struct
        type t = outer_t

        let zero = []

        let one = lift Value.one Monomial_.one

        let neg poly =
          mult_with_const (Value.neg Value.one) poly

        let add poly1 poly2 =
          simplify (List.append poly1 poly2)

        let mul poly1 poly2 =
             List.cartesian_product poly1 poly2
          |> List.map ~f:(fun (a, b) -> ScaledMonomial_.mul a b)

        let pow poly d =
          Util.iterate_n_times (mul poly) d one

      end
    include PolyTypes.MakeMath(BaseMathImpl)

    module BasePartialOrderImpl : (PolyTypes.BasePartialOrder with type t = outer_t) =
      struct
        type t = outer_t

        let rec equal_simplified poly1 poly2 =
          List.length poly1 == List.length poly2 &&
            match poly1 with
            | [] -> true
            | scaled :: tail ->
               let curr_mon = ScaledMonomial_.monomial scaled in
               let curr_coeff = ScaledMonomial_.coeff scaled in
               Value.(=~=) curr_coeff (coeff curr_mon poly2) &&
                 equal_simplified tail (delete_monomial curr_mon poly2)

        let (=~=) poly1 poly2 =
          equal_simplified (simplify poly1) (simplify poly2)

        let (>) p1 p2 = match (p1, p2) with
          (* TODO Find some rules to compare polynomials *)
          | ([s1], [s2]) -> ScaledMonomial_.(>) s1 s2
          | _ -> None

      end
    include PolyTypes.MakePartialOrder(BasePartialOrderImpl)

    let is_zero poly = poly =~= zero

    let is_one poly = poly =~= one

    let instantiate (substitution : Value.t -> t) =
      fold ~const:substitution ~indeterminate:of_indeterminate ~neg:neg ~plus:add ~times:mul ~pow:pow

    let eval_f poly f =
         poly
      |> List.map ~f:(fun scaled -> ScaledMonomial_.eval_f scaled f)
      |> List.fold_left ~f:Value.add ~init:Value.zero

    let eval poly valuation =
         poly
      |> List.map ~f:(fun scaled -> ScaledMonomial_.eval scaled valuation)
      |> List.fold_left ~f:Value.add ~init:Value.zero

    let substitute_f substitution =
      fold ~const:of_constant ~indeterminate:substitution ~neg:neg ~plus:add ~times:mul ~pow:pow

    let substitute ind ~replacement =
      substitute_f (fun target_ind ->
          if I.equal ind target_ind then replacement else of_indeterminate target_ind
        )

    let eval_partial poly valuation =
      substitute_f (fun ind ->
          Option.map ~f:of_constant (Valuation_.eval_opt ind valuation) |? of_indeterminate ind
        ) poly

    let partition f =
      List.partition_tf ~f

  end

module PolynomialOver(Value: PolyTypes.Ring) = struct
  include PolynomialOverIndeterminate(VarIndeterminate)(Value)

  let substitute_all substitution t =
    substitute_f (fun var ->
        Option.value ~default:(of_var var) @@ Map.find substitution var
      ) t

end


module Polynomial =
  struct
    include PolynomialOver(OurInt)

    let separate_by_sign poly =
      partition (fun scaled -> OurInt.Compare.(ScaledMonomial_.coeff scaled >= OurInt.zero)) poly

    let max_of_occurring_constants =
      fold
        ~const:OurInt.abs
        ~indeterminate:(fun _ -> OurInt.one)
        ~neg:identity
        ~plus:OurInt.add
        ~times:OurInt.mul
        ~pow:OurInt.pow

    let pull_out_common_addends (t1:t) (t2:t)  =
      let rec remove_at pos = function
        | [] -> []
        | (x::xs) -> Int.(if pos > 0 then x :: remove_at (pos - 1) xs else xs)
      in
      (* Iterate over t1. Initially t2' = t1. *)
      (* In every step we take a scaled monomial from t1. *)
      (* If the monomial occurs in t2 as well (with a coefficient of the same sign) we pull out the scaled monomial by adding it to t', t1' (if necessary), and altering t2' *)
      let iter_t1 (t',(t1',t2')) t1_next =
        let next_mon = ScaledMonomial_.monomial t1_next in
        let t2_o =
          List.findi
            ~f:(fun _ t2_next -> Monomial_.equal next_mon (ScaledMonomial_.monomial t2_next)
                         &&  OurInt.compare (ScaledMonomial_.coeff t1_next) OurInt.zero = OurInt.compare (ScaledMonomial_.coeff t2_next) OurInt.zero)
            t2'
        in
        match t2_o with
        | None -> t',(t1_next::t1',t2')
        | Some (i,t2_next) ->
          let c1, c2 = ScaledMonomial_.coeff t1_next, ScaledMonomial_.coeff t2_next in
          let common_mon = ScaledMonomial_.monomial t1_next in
          match OurInt.(compare (abs c1) (abs c2)) with
            | -1 ->
              let t' = ScaledMonomial_.make c1 common_mon :: t' in
              let t2' = ScaledMonomial_.make OurInt.(c2 - c1) common_mon :: remove_at i t2' in
              t', (t1',t2')
            | _  ->
              let t' = ScaledMonomial_.make c2 common_mon :: t' in
              let t1' = ScaledMonomial_.make OurInt.(c1 - c2) common_mon :: t1' in
              let t2' = remove_at i t2' in
              t', (t1',t2')
      in
      List.fold ~f:iter_t1 ~init:([],([],t2)) t1

  end

module RealPolynomial =
  struct
    include PolynomialOver(OurFloat)

    let separate_by_sign poly =
      partition (fun scaled -> OurFloat.Compare.(ScaledMonomial_.coeff scaled >= OurFloat.zero)) poly

    let max_of_occurring_constants =
      fold
        ~const:OurFloat.abs
        ~indeterminate:(fun _ -> OurFloat.one)
        ~neg:identity
        ~plus:OurFloat.add
        ~times:OurFloat.mul
        ~pow:OurFloat.pow

    let of_intpoly  =
      Polynomial.fold ~const:(of_constant % OurFloat.of_ourint) ~indeterminate:(of_var) ~neg:neg ~plus:add ~times:mul ~pow:pow

    let of_intconstant = of_constant % OurFloat.of_ourint
  end
module RationalPolynomial =
  struct
    include PolynomialOver(OurRational)

    let normalize poly =
      let coeff_inv =
        coeffs poly
        |> List.filter ~f:(not % OurRational.is_integer)
        |> List.map ~f:Tuple2.second
        |> List.fold ~f:OurInt.lcm ~init:OurInt.one |> OurInt.abs |> OurRational.of_ourint in
      poly
      |> mult_with_const coeff_inv
      |> fold
        ~const:(Polynomial.of_constant % OurRational.to_ourint)
        ~indeterminate:(Polynomial.of_var)
        ~neg:Polynomial.neg
        ~plus:Polynomial.add
        ~times:Polynomial.mul
        ~pow:Polynomial.pow

    let normalize_return_factor poly =
      let coeff_inv =
        coeffs poly
        |> List.filter ~f:(not % OurRational.is_integer)
        |> List.map ~f:Tuple2.second
        |> List.fold ~f:OurInt.lcm ~init:OurInt.one |> OurInt.abs |> OurRational.of_ourint in
      (poly
      |> mult_with_const coeff_inv
      |> fold
        ~const:(Polynomial.of_constant % OurRational.to_ourint)
        ~indeterminate:(Polynomial.of_var)
        ~neg:Polynomial.neg
        ~plus:Polynomial.add
        ~times:Polynomial.mul
        ~pow:Polynomial.pow, coeff_inv)

    let overapprox =
      fold
        ~const:(Polynomial.of_constant % OurRational.ceil % OurRational.abs)
        ~indeterminate:Polynomial.of_var
        ~neg:Polynomial.neg
        ~plus:Polynomial.add
        ~times:Polynomial.mul
        ~pow:Polynomial.pow

    let of_intpoly  =
      Polynomial.fold ~const:(of_constant % OurRational.of_ourint) ~indeterminate:(of_var) ~neg:neg ~plus:add ~times:mul ~pow:pow

    let is_integer_poly  poly =
      List.for_all ~f:OurRational.is_integer @@ coeffs poly

  end

module ParameterPolynomialOver(Value : PolyTypes.Ring) = struct
  module Outer = PolynomialOver(PolynomialOver(Value))
  module Inner = PolynomialOver(Value)

  include Outer

  let eval_coefficients f =
    Outer.fold ~const:(fun inner -> Inner.of_constant (Inner.eval_f inner f))
               ~indeterminate:Inner.of_var
               ~neg:Inner.neg
               ~plus:Inner.add
               ~times:Inner.mul
               ~pow:Inner.pow

  (** Transforms the template polynomial such that all inner values get lifted to the outer polynomial. *)
  (** Example: (2a+b)x + (3a)y - 1 gets transformed to 2ax + bx + 3ay - 1 *)
  let flatten (templatepoly : Outer.t): Inner.t =
    Outer.fold ~const:identity ~indeterminate:Inner.of_var ~neg:Inner.neg ~plus:Inner.add ~times:Inner.mul ~pow:Inner.pow templatepoly

  (** Lifts a polynomial to a parameter polynomial such that the inner structure is kept.*)
  (** Example: 2x +3 is interpreted as 2x+3 and not as the constant polynomial (2x+3)*(1)*)
  let of_polynomial (poly : Inner.t): t =
    Inner.fold ~const:(fun value -> of_constant (Inner.of_constant value)) ~indeterminate:of_var ~neg:neg ~plus:add ~times:mul ~pow:pow poly
end

module ParameterPolynomial = ParameterPolynomialOver(OurInt)

module RealParameterPolynomial = ParameterPolynomialOver(OurFloat)
