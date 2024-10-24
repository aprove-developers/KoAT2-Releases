open Batteries
open Polynomials

module ConstantConstraint = struct
  module Comparator = struct
    type t = EQ | NEQ

    let to_string = function
      | EQ -> "=="
      | NEQ -> "!="
  end

  type atom = Comparator.t * OurInt.t

  let atom_eq = function
    | Comparator.EQ, _ -> true
    | _ -> false


  type t = C of atom list | T | F

  let mk c = function
    | Comparator.EQ -> C [ (Comparator.EQ, c) ]
    | Comparator.NEQ -> C [ (Comparator.NEQ, c) ]


  let mk_eq c = mk c Comparator.EQ
  let mk_neq c = mk c Comparator.NEQ
  let mk_true = T
  let mk_false = F

  let equal_atom (comp1, cons1) (comp2, cons2) =
    match (comp1, comp2) with
    | Comparator.EQ, Comparator.EQ -> OurInt.equal cons1 cons2
    | Comparator.NEQ, Comparator.NEQ -> OurInt.equal cons1 cons2
    | _ -> false


  let comp_atom (comp1, cons1) (comp2, cons2) =
    let diff = OurInt.to_int cons1 - OurInt.to_int cons2 in
    if diff != 0 then
      diff
    else
      match (comp1, comp2) with
      | Comparator.EQ, Comparator.NEQ -> -1
      | Comparator.NEQ, Comparator.EQ -> 1
      | _ -> 0


  let remove_duplicates = function
    | T -> T
    | F -> F
    | C atoms -> C (List.sort_uniq comp_atom atoms)


  let simplify = function
    | T -> T
    | F -> F
    | C [] -> T
    | C atoms ->
        let exist_contradiction =
          List.cartesian_product atoms atoms
          |> List.exists (fun ((comp1, cons1), (comp2, cons2)) ->
                 match (comp1, comp2) with
                 | Comparator.EQ, Comparator.EQ -> not (OurInt.equal cons1 cons2)
                 | Comparator.EQ, Comparator.NEQ -> OurInt.equal cons1 cons2
                 | Comparator.NEQ, Comparator.EQ -> OurInt.equal cons1 cons2
                 | _ -> false)
        in
        if exist_contradiction then
          F
        else
          C atoms |> remove_duplicates


  let equal c1 c2 =
    match (simplify c1, simplify c2) with
    | T, T -> true
    | F, F -> true
    | C atoms1, C atoms2 ->
        List.length atoms1 = List.length atoms2
        && List.for_all (fun (a, b) -> equal_atom a b) (List.combine atoms1 atoms2)
    | _ -> false


  let mk_and const1 const2 =
    match const1 with
    | T -> const2
    | F -> F
    | C atoms1 -> (
        match const2 with
        | T -> const1
        | F -> F
        | C atoms2 -> simplify (C (atoms1 @ atoms2)))


  let mk_gt c =
    let rec helper_ = function
      | 0 -> [ (Comparator.NEQ, OurInt.zero) ]
      | c -> (Comparator.NEQ, OurInt.of_int c) :: helper_ (c - 1)
    in
    C (helper_ c)


  let is_false = function
    | F -> true
    | _ -> false


  let is_true = function
    | T -> true
    | _ -> false


  let rec compare_ atoms1 atoms2 =
    match (atoms1, atoms2) with
    | [], [] -> 0
    | x :: xs, y :: ys when comp_atom x y != 0 -> comp_atom x y
    | x :: xs, y :: ys -> compare_ xs ys
    | [], y :: ys -> -1
    | x :: xs, [] -> 1


  let increase c = function
    | T -> T
    | F -> F
    | C atoms -> C (List.map (fun (comp, cons) -> (comp, OurInt.add cons c)) atoms)


  let to_string = function
    | T -> "[[true]]"
    | F -> "[[false]]"
    | C [] -> "[[true]]"
    | C ((comp, c) :: xs) ->
        "[[n " ^ Comparator.to_string comp ^ " " ^ OurInt.to_string c
        ^ List.fold_right
            (fun (comp, c) str -> ", n " ^ Comparator.to_string comp ^ " " ^ OurInt.to_string c ^ str)
            xs ""
        ^ "]]"


  let compare c1 c2 =
    match (c1, c2) with
    | C atoms1, C atoms2 -> compare_ atoms1 atoms2
    | T, F -> -1
    | F, T -> 1
    | C _, _ -> 1
    | _, C _ -> -1
    | _ -> 0


  let contains_positive = function
    | C atoms ->
        let option = List.find_opt atom_eq atoms in
        if Option.is_some option then
          option |> Option.get |> Tuple2.second |> Option.some
        else
          None
    | _ -> None


  let eval i = function
    | T -> true
    | F -> false
    | C atoms ->
        List.for_all
          (fun (comp, c) ->
            match comp with
            | Comparator.EQ -> OurInt.equal c i
            | Comparator.NEQ -> OurInt.equal c i |> Bool.not)
          atoms


  let max_constant = function
    | C [] -> OurInt.minus_one
    | C atoms -> OurInt.max_list (List.map Tuple2.second atoms)
    | _ -> OurInt.minus_one
end

module type IntSupRing = sig
  type t

  include PolyTypes.Math with type t := t
  include PolyTypes.Ring with type t := t

  val of_ourint : OurInt.t -> t
  val div : t -> t -> t
end

module PE (Value : IntSupRing) = struct
  module Polynomial = PolynomialOver (Value)
  module ScaledMonomial = ScaledMonomials.Make (Value)
  module Monomial = Monomials.Make (Value)

  type t = (ConstantConstraint.t * Polynomial.t * int * Value.t) list
  (** A  Polynomial Expression has the form \sum_j constraint(n) * poly(vars) * n^a * b^n *)

  let mk constr poly degree base : t = [ (constr, poly, degree, base) ]

  let mk_cons cons =
    if Value.equal Value.zero cons then
      []
    else
      [ (ConstantConstraint.mk_true, Polynomial.of_constant cons, 0, Value.one) ]


  let mk_var var = [ (ConstantConstraint.mk_true, Polynomial.of_var var, 0, Value.one) ]

  let to_string pe =
    match pe with
    | [] -> "0"
    | xs ->
        let to_string_cc (c, p, d, b) =
          if ConstantConstraint.is_true c then
            if d = 0 && Value.(equal one b) && Polynomial.is_one p then
              "1"
            else
              ""
          else
            ConstantConstraint.to_string c
        in
        let to_string_poly p =
          if Polynomial.is_one p then
            ""
          else if p |> Polynomial.monomials |> List.length |> ( < ) 1 then
            "(" ^ Polynomial.to_string p ^ ")"
          else
            Polynomial.to_string p
        in
        let to_string_poly_n d =
          if d = 0 then
            ""
          else
            "n^" ^ string_of_int d
        in
        let to_string_exp_n b =
          if Value.(equal one b) then
            ""
          else
            "(" ^ Value.to_string b ^ ")^n"
        in
        xs
        |> List.map (fun (c, p, d, b) ->
               [ to_string_cc (c, p, d, b); to_string_poly p; to_string_poly_n d; to_string_exp_n b ]
               |> List.filter (not % String.equal "")
               |> String.concat " * ")
        |> List.filter (not % String.equal "")
        |> String.concat " + "


  let to_string_pretty pe =
    match pe with
    | [] -> "0"
    | xs ->
        let to_string_cc (c, p, d, b) =
          if ConstantConstraint.is_true c then
            if d = 0 && Value.(equal one b) && Polynomial.is_one p then
              "1"
            else
              ""
          else
            ConstantConstraint.to_string c
        in
        let to_string_poly p =
          if Polynomial.is_one p then
            ""
          else if p |> Polynomial.monomials |> List.length |> ( < ) 1 then
            "(" ^ Polynomial.to_string_pretty p ^ ")"
          else
            Polynomial.to_string_pretty p
        in
        let to_string_poly_n d =
          if d = 0 then
            ""
          else
            "n^" ^ string_of_int d
        in
        let to_string_exp_n b =
          if Value.(equal one b) then
            ""
          else
            "(" ^ Value.to_string b ^ ")^n"
        in
        xs
        |> List.map (fun (c, p, d, b) ->
               [ to_string_cc (c, p, d, b); to_string_poly p; to_string_poly_n d; to_string_exp_n b ]
               |> List.filter (not % String.equal "")
               |> String.concat "⋅")
        |> List.filter (not % String.equal "")
        |> String.concat " + "


  let compare (c1, p1, d1, b1) (c2, p2, d2, b2) =
    if ConstantConstraint.compare c1 c2 != 0 then
      ConstantConstraint.compare c1 c2
    else if not @@ Value.equal b1 b2 then
      Value.compare b2 b1
    else if d2 - d1 != 0 then
      d2 - d1
    else
      0


  let simplify pe =
    List.group compare pe
    |> List.map (fun list ->
           List.fold_right
             (fun (c, p, d, b) (c1, p1, d1, b1) -> (c, Polynomial.add p p1, d, b))
             list
             (ConstantConstraint.mk_true, Polynomial.of_constant Value.zero, 0, Value.one))
    |> List.filter (fun (c, p, d, b) -> not (Polynomial.is_zero p))
    |> List.map (fun (c, p, d, b) -> (ConstantConstraint.simplify c, p, d, b))
    |> List.filter (fun (c, p, d, b) -> not (ConstantConstraint.is_false c))
    |> List.sort compare


  let add = List.append % simplify

  let mul pe1 pe2 =
    List.cartesian_product pe1 pe2
    |> List.map (fun ((c1, p1, d1, b1), (c2, p2, d2, b2)) ->
           (ConstantConstraint.mk_and c1 c2, Polynomial.mul p1 p2, d1 + d2, Value.(b1 * b2)))
    |> simplify


  let rec power pe exponent =
    match exponent with
    | 0 -> []
    | 1 -> pe
    | n -> mul pe (power pe (exponent - 1))


  let rec power_list (pe_exp_list : (t * int) list) =
    match pe_exp_list with
    | [] -> []
    | [ (pe, exp) ] -> power pe exp
    | (pe, exp) :: xs -> mul (power pe exp) (power_list xs)


  let substitute varmap poly =
    Polynomial.fold ~const:mk_cons
      ~indeterminate:(fun var ->
        try Hashtbl.find varmap var with
        | Not_found -> [])
      ~plus:add ~times:mul ~pow:power poly
    |> simplify


  let substitute_f f = List.map (Tuple4.map2 @@ Polynomial.substitute_f f)

  let rec binomial n k =
    if n = k then
      OurInt.one
    else
      OurInt.div (OurInt.mul (binomial (n - 1) k) (OurInt.of_int n)) (OurInt.of_int (n - k))


  (**  Computes for (n - 1)^d the normal-form, i.e., \sum_i=0^d (d choose i) n^i (-1)^{d-i}. *)
  let transform d =
    let n = Var.of_string "n" in
    List.fold_right
      (fun i p ->
        let coeff =
          let exp = (d - i) mod 2 in
          OurInt.(pow (of_int (-1)) exp * binomial d i) |> Value.of_ourint
        in
        let poly =
          match i with
          | 0 -> Polynomial.of_constant coeff
          | _ -> Polynomial.mult_with_const coeff (Polynomial.of_power n i)
        in
        Polynomial.add p poly)
      (List.range 0 `To d) Polynomial.zero


  module ClosedFormTable = Hashtbl.Make (Var)

  let closed_form_table : t ClosedFormTable.t = ClosedFormTable.create 10

  (* c.f.: masterthesis *)
  let insert_previous_cf var poly monomials =
    let scaled_monomials = Polynomial.scaled_monomials poly in
    List.fold_right add
      (List.map
         (fun scaled_monom ->
           let vars = scaled_monom |> Base.Set.to_list % ScaledMonomial.vars in
           let monom = ScaledMonomial.monomial scaled_monom in
           let constant = (mk_cons (ScaledMonomial.coeff scaled_monom), 1) in
           vars
           |> List.map (fun var ->
                  (ClosedFormTable.find closed_form_table var, Monomial.degree_variable var monom))
           |> List.append [ constant ] |> power_list)
         scaled_monomials)
      []
    |> simplify


  (* c.f.: eq. (6) *)
  let only_poly var poly =
    let monomials = Polynomial.monomials poly in
    let pe = insert_previous_cf var poly monomials in
    let pe_ =
      List.fold_right
        (fun (c, p, d, b) pe ->
          add
            (List.map
               (fun i ->
                 let coeff =
                   let exp = (d - i) mod 2 in
                   let term = OurInt.(pow (of_int (-1)) exp * binomial d i) |> Value.of_ourint in
                   Value.div term b
                 in
                 ( ConstantConstraint.mk_and
                     (ConstantConstraint.increase OurInt.one c)
                     (ConstantConstraint.mk_neq OurInt.zero),
                   Polynomial.mult_with_const coeff p,
                   i,
                   b ))
               (List.range 0 `To d))
            pe)
        pe []
    in
    [ (ConstantConstraint.mk_eq OurInt.zero, Polynomial.of_var var, 0, Value.one) ] @ pe_


  (* c.f.: eq. (7) *)
  let positive (c, p, d, b) pos_const coeff_var =
    if ConstantConstraint.is_false c then
      []
    else
      let pos_const_int = OurInt.to_int pos_const in
      let coeff =
        Value.(
          div
            (mul (pow (Value.of_ourint pos_const) d) (pow b pos_const_int))
            (pow coeff_var OurInt.(to_int (pos_const + one))))
      in
      [ (ConstantConstraint.mk_gt pos_const_int, Polynomial.mult_with_const coeff p, 0, coeff_var) ]


  let rec compute_r q c =
    let n = Var.of_string "n" in
    if Polynomial.is_const q then
      if Value.equal Value.one c then
        Polynomial.of_coeff_list [ Polynomial.get_constant q ] [ n ]
      else
        Polynomial.of_constant Value.(div (Polynomial.get_constant q) (one - c))
    else
      let d = Polynomial.degree q in
      let c_d = q |> Polynomial.degree_coeff_list |> List.last in
      if Value.equal Value.one c then
        let s =
          Polynomial.mult_with_const (Value.div c_d (Value.of_int (d + 1))) (Polynomial.of_power n (d + 1))
        in
        let s_ = Polynomial.mult_with_const (Value.div c_d (Value.of_int (d + 1))) (transform (d + 1)) in
        Polynomial.add s (compute_r Polynomial.(add (sub q s) (mult_with_const c s_)) c)
      else
        let s = Polynomial.mult_with_const Value.(div c_d (one - c)) (Polynomial.of_power n d) in
        let s_ = Polynomial.mult_with_const Value.(div c_d (one - c)) (transform d) in
        Polynomial.add s (compute_r Polynomial.(add (sub q s) (mult_with_const c s_)) c)


  (* c.f.: eq. (8) *)
  let no_positive (c, p, d, b) max_const coeff_var =
    if ConstantConstraint.is_false c then
      []
    else
      (* c.f.: eq. (9) *)
      let rec le_max = function
        | -1 -> []
        | 0 -> []
        | i ->
            let tmp = i - 1 in
            let term =
              if ConstantConstraint.eval (OurInt.of_int tmp) c && i >= 1 then
                let coeff = Value.(div (mul (pow (of_int tmp) d) (pow b tmp)) (pow coeff_var i)) in
                [ (ConstantConstraint.mk_gt tmp, Polynomial.mult_with_const coeff p, 0, coeff_var) ]
              else
                []
            in
            term @ le_max tmp
      in
      (* c.f.: eq. (10) *)
      let gt_max =
        let r = compute_r (transform d) Value.(div coeff_var b) in
        let max_const_inc = OurInt.(max_const + one) in
        let coeff =
          Value.(mul (div (pow (div b coeff_var) (OurInt.to_int max_const_inc)) (neg b)))
            (Polynomial.eval_f r (fun _ -> Value.of_ourint max_const_inc))
        in
        let coeff_list = Polynomial.degree_coeff_list r in
        let degree_r = List.length coeff_list - 1 in
        let rec pe_ coeff_list_ i =
          let term x =
            [
              ( ConstantConstraint.mk_gt (OurInt.to_int max_const + 1),
                Polynomial.mult_with_const (Value.div x b) p,
                degree_r - i,
                b );
            ]
          in
          match coeff_list_ with
          | [] -> []
          | x :: xs -> term x @ pe_ xs (i - 1)
        in
        [
          ( ConstantConstraint.mk_gt (OurInt.to_int max_const + 1),
            Polynomial.mult_with_const coeff p,
            0,
            coeff_var );
        ]
        @ pe_ coeff_list degree_r
      in
      le_max (OurInt.to_int max_const + 1) @ gt_max


  let poly_with_var var poly =
    let monomials =
      Polynomial.monomials poly |> List.filter (fun monomial -> Monomial.degree_variable var monomial = 0)
    in
    let coeff_var = poly |> Polynomial.coeff_of_var var in
    let pe =
      insert_previous_cf var (Polynomial.sub poly (Polynomial.of_coeff_list [ coeff_var ] [ var ])) monomials
    in
    let pe_ =
      List.fold_right
        (fun (c, p, d, b) tmp ->
          let pos_const = ConstantConstraint.contains_positive c in
          if Option.is_some pos_const then
            positive (c, p, d, b) (Option.get pos_const) coeff_var @ tmp
          else
            no_positive (c, p, d, b) (ConstantConstraint.max_constant c) coeff_var @ tmp)
        pe []
    in
    [ (ConstantConstraint.mk_true, Polynomial.of_var var, 0, coeff_var) ] @ pe_


  let normalize pe_list =
    pe_list
    |> List.map (fun pe ->
           List.map
             (fun (c, p, d, b) ->
               if Option.is_some (ConstantConstraint.contains_positive c) then
                 (ConstantConstraint.mk_false, p, d, b)
               else
                 (ConstantConstraint.mk_true, p, d, b))
             pe
           |> simplify)


  let max_const = function
    | [] -> OurInt.zero
    | xs -> OurInt.max_list (List.map (ConstantConstraint.max_constant % Tuple4.first) xs)


  (** We assume that we get a list of var -> poly, s.t., the list ordering corresponds to a valid twn order. *)
  let compute_closed_form var_poly =
    ClosedFormTable.clear closed_form_table;
    List.map
      (fun (var, poly) ->
        let tmp =
          if Base.Set.mem (Polynomial.vars poly) var then
            poly_with_var var poly
          else
            only_poly var poly
        in
        tmp |> simplify |> tap (fun pe -> ClosedFormTable.add closed_form_table var pe))
      var_poly
end

module RationalPE = struct
  include PE (OurRational)

  let remove_frac pe =
    let lcm =
      List.map (Polynomial.coeffs % Tuple4.second) pe
      |> List.concat |> List.map OurRational.den |> OurInt.lcm_list
    in
    List.map (fun (c, p, d, b) -> (c, Polynomial.mult_with_const (OurRational.of_ourint lcm) p, d, b)) pe


  open Bounds

  let overapprox t runtime_bound =
    List.map
      (fun (c, p, d, b) ->
        let bound_p = Bound.of_poly @@ RationalPolynomial.overapprox p
        and bound_d = Bound.pow runtime_bound d
        and bound_b = Bound.exp (OurRational.ceil b) runtime_bound in
        Bound.(bound_p * bound_d * bound_b))
      t
    |> Bound.sum_list
end

module ComplexPE = struct
  include PE (OurAlgebraicComplex)
  open Bounds

  let to_bound t =
    let n = Var.of_string "n" in
    List.map
      (fun (c, p, d, b) ->
        let bound_p = Bound.of_poly @@ CAPolynomial.overapprox p
        and bound_d = Bound.(pow (Bound.of_var n) d)
        and bound_b = Bound.exp (OurAlgebraic.ceil @@ OurAlgebraicComplex.abs b) (Bound.of_var n) in
        Bound.(bound_p * bound_d * bound_b))
      t
    |> Bound.sum_list
end
