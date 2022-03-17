open Batteries
open Polynomials
open ProgramTypes
open Formulas
open Constraints
open Atoms
open Util

module ConstantConstraint = struct
    module Comparator =
      struct
        type t = EQ | NEQ [@@deriving eq, ord]

        let values = [EQ; NEQ]

        let to_string = function
        | EQ -> "=="
        | NEQ -> "!="

      end

    type atom = (Comparator.t * OurInt.t) [@@deriving eq, ord]

    let atom_eq = function
    | (Comparator.EQ, _) -> true
    | _ -> false

    let atom_neq = function
    | (Comparator.NEQ, _) -> true
    | _ -> false

    type t = C of atom list | T | F [@@deriving eq, ord]

    let mk c = function
    | Comparator.EQ -> C [(Comparator.EQ, c)]
    | Comparator.NEQ -> C [(Comparator.NEQ, c)]

    let mk_eq c = mk c Comparator.EQ
    let mk_neq c = mk c Comparator.NEQ
    let mk_true = T
    let mk_false = F

    let equal_atom (comp1, cons1) (comp2, cons2) = match (comp1, comp2) with
    | (Comparator.EQ, Comparator.EQ) -> OurInt.equal cons1 cons2
    | (Comparator.NEQ, Comparator.NEQ) -> OurInt.equal cons1 cons2
    | _ -> false

    let comp_atom (comp1, cons1) (comp2, cons2) =  let diff =  ((OurInt.to_int cons1) - (OurInt.to_int cons2)) in
        if diff != 0
            then diff
        else match (comp1, comp2) with
            | (Comparator.EQ, Comparator.NEQ) -> -1
            | (Comparator.NEQ, Comparator.EQ) -> 1
            | _ -> 0

    let remove_duplicates = function
    | T -> T
    | F -> F
    | C atoms -> C (List.sort_uniq comp_atom atoms)

    let simplify = function
    | T -> T
    | F -> F
    | C [] -> T
    | C atoms -> let exist_contradiction =
        List.cartesian_product atoms atoms
        |> List.exists (fun ((comp1, cons1),(comp2, cons2)) ->
            match (comp1, comp2) with
            | (Comparator.EQ, Comparator.EQ) -> not (OurInt.equal cons1 cons2)
            | (Comparator.EQ, Comparator.NEQ) -> OurInt.equal cons1 cons2
            | (Comparator.NEQ, Comparator.EQ) -> OurInt.equal cons1 cons2
            | _ -> false) in
        if exist_contradiction then F else (C atoms |> remove_duplicates)

    let equal c1 c2 = match (simplify c1, simplify c2) with
    | (T,T) -> true
    | (F,F) -> true
    | (C atoms1, C atoms2) -> ((List.length atoms1) = (List.length atoms2)) && List.for_all (fun (a,b) -> equal_atom a b) (List.combine atoms1 atoms2)
    | _ -> false

    let mk_and const1 const2 = match const1 with
    | T -> const2
    | F -> F
    | C atoms1 ->
        match const2 with
        | T -> const1
        | F -> F
        | C atoms2 -> simplify (C (atoms1 @ atoms2))

    let mk_gt c =
        let rec helper_ = function
            | 0 -> [(Comparator.NEQ, OurInt.zero)]
            | c -> (Comparator.NEQ, OurInt.of_int c)::(helper_ (c - 1)) in
        C (helper_ c)

    let is_false = function
    | F -> true
    | _ -> false

    let is_true = function
    | T -> true
    | _ -> false

    let rec compare_ atoms1 atoms2 = match (atoms1, atoms2) with
    | ([], []) -> 0
    | (x::xs, y::ys) when (comp_atom x y != 0) -> comp_atom x y
    | (x::xs, y::ys) -> compare_ xs ys
    | ([], y::ys) -> -1
    | (x::xs, []) -> 1

    let increase c = function
    | T -> T
    | F -> F
    | C atoms -> C (List.map (fun (comp, cons) -> (comp, OurInt.add cons c)) atoms)

    let to_string = function
    | T -> "[[true]]"
    | F -> "[[false]]"
    | C [] -> "[[true]]"
    | C ((comp, c)::xs) ->  "[[n " ^ (Comparator.to_string comp) ^ " " ^ (OurInt.to_string c) ^ (List.fold_right (fun (comp, c) str -> ", n " ^ (Comparator.to_string comp) ^ " " ^ (OurInt.to_string c) ^ str)
                 xs
                 "") ^ "]]"

    let compare c1 c2 =
    match (c1, c2) with
    | (T,F) -> -1
    | (F,T) -> 1
    | (C _ , T) -> 1
    | (C _ , F) -> 1
    | (T, C _) -> -1
    | (F , C _) -> -1
    | (C atoms1, C atoms2) -> compare_ atoms1 atoms2
    | (T,T) -> 0
    | (F,F) -> 0

    let contains_positive = function
    | C atoms -> let option = List.find_opt atom_eq atoms in
                    if Option.is_some option then option |> Option.get |> Tuple2.second |> Option.some
                    else None
    | _ -> None

    let eval i = function
    | T -> true
    | F -> false
    | C atoms -> List.for_all (fun (comp, c) -> match comp with
                                                | Comparator.EQ -> OurInt.equal c i
                                                | Comparator.NEQ -> OurInt.equal c i |> Bool.not) atoms

    let max_constant = function
    | C [] -> OurInt.minus_one
    | C atoms -> OurInt.max_list (List.map Tuple2.second atoms)
    | _ -> OurInt.minus_one
end

module PE = struct
    (** A  Polynomial Expression has the form \sum_j constraint(n) * poly(vars) * n^a * b^n *)
    type t = (ConstantConstraint.t * RationalPolynomial.t * int * int) list

    module ScaledMonomial = ScaledMonomials.Make(OurInt)
    module Monomial = Monomials.Make(OurInt)

    let mk constr poly degree base = [(constr, poly, degree, base)]

    let mk_cons cons = if OurRational.equal OurRational.zero cons then [] else [(ConstantConstraint.mk_true, RationalPolynomial.of_constant cons, 0, 1)]

    let to_string pe = match pe with
        | [] -> "0"
        | xs ->
            let to_string_cc (c, p, d, b) = if ConstantConstraint.is_true c then (if (d = 0 && b = 1 && RationalPolynomial.is_one p) then "1" else "") else (ConstantConstraint.to_string c) in
            let to_string_poly p = if RationalPolynomial.is_one p then ""
                                   else if (p |> RationalPolynomial.monomials |> List.length |> (<) 1) then  "(" ^ (RationalPolynomial.to_string p) ^ ")"
                                   else (RationalPolynomial.to_string p) in
            let to_string_poly_n d = if d = 0 then "" else "n^" ^ (string_of_int d) in
            let to_string_exp_n b = if b = 1 then "" else (string_of_int b) ^ "^n" in
            xs
            |> List.map (fun (c, p, d, b) -> [to_string_cc (c, p, d, b); to_string_poly p; to_string_poly_n d; to_string_exp_n b] |> List.filter (not % ((String.equal) "")) |> String.concat " * ")
            |> List.filter (not % ((String.equal) ""))
            |> String.concat " + "

    let to_string_pretty pe = match pe with
    | [] -> "0"
    | xs ->
        let to_string_cc (c, p, d, b) = if ConstantConstraint.is_true c then (if (d = 0 && b = 1 && RationalPolynomial.is_one p) then "1" else "") else (ConstantConstraint.to_string c) in
        let to_string_poly p = if RationalPolynomial.is_one p then ""
                                else if (p |> RationalPolynomial.monomials |> List.length |> (<) 1) then  "(" ^ (RationalPolynomial.to_string_pretty p) ^ ")"
                                else (RationalPolynomial.to_string_pretty p) in
        let to_string_poly_n d = if d = 0 then "" else "n^" ^ (string_of_int d) in
        let to_string_exp_n b = if b = 1 then "" else (string_of_int b) ^ "^n" in
        xs
        |> List.map (fun (c, p, d, b) -> [to_string_cc (c, p, d, b); to_string_poly p; to_string_poly_n d; to_string_exp_n b] |> List.filter (not % ((String.equal) "")) |> String.concat " * ")
        |> List.filter (not % ((String.equal) ""))
        |> String.concat " + "

    let compare (c1,p1,d1,b1) (c2,p2,d2,b2) =
        if ConstantConstraint.compare c1 c2 != 0 then ConstantConstraint.compare c1 c2
        else if b2 - b1 != 0 then b2 - b1
        else if d2 - d1 != 0 then d2 - d1
        else 0

    let simplify pe =
        List.group compare pe
        |> List.map (fun list ->
            List.fold_right (fun (c, p, d, b) (c1, p1, d1, b1) -> (c, RationalPolynomial.add p p1, d, b))
            list
            (ConstantConstraint.mk_true, RationalPolynomial.of_constant (OurRational.zero), 0, 1))
        |> List.filter (fun (c, p, d, b) -> not (RationalPolynomial.is_zero p))
        |> List.map (fun (c, p, d, b) -> (ConstantConstraint.simplify c, p, d, b))
        |> List.filter (fun (c, p, d, b) -> not (ConstantConstraint.is_false c))
        |> List.sort compare

    let add = List.append % List.filter (fun (c, p, d, b) -> not (RationalPolynomial.is_zero p))

    let mul pe1 pe2 =
        List.cartesian_product pe1 pe2
        |> List.map (fun ((c1, p1, d1, b1), (c2, p2, d2, b2)) -> (ConstantConstraint.mk_and c1 c2, RationalPolynomial.mul p1 p2, d1 + d2, b1 * b2))
        |> List.filter (fun (c, p, d, b) -> not (ConstantConstraint.is_false c))

    let rec power pe exponent =  match exponent with
        | 0 -> []
        | 1 -> pe
        | n -> (mul pe (power pe (exponent - 1)))

    let rec power_list (pe_exp_list: (t * int) list) =  match pe_exp_list with
        | [] -> []
        | [(pe,exp)] -> power pe exp
        | (pe,exp)::xs -> mul (power pe exp) (power_list xs)

    let neg =
        List.map (fun (c, p, d, b) -> (c, RationalPolynomial.neg p, d, b))

    let substitute varmap poly =
    Polynomial.fold ~const:(mk_cons % OurRational.of_ourint)
                    ~var:(fun var -> Hashtbl.find varmap var)
                    ~neg:neg
                    ~plus:add
                    ~times:mul
                    ~pow:power poly
    |> simplify

    let rec binomial n k = if n = k then OurInt.one
                                    else OurInt.div (OurInt.mul (binomial (n-1) k) (OurInt.of_int n)) (OurInt.of_int (n-k))

    (**  Computes for (n - 1)^d the normal-form, i.e., \sum_i=0^d (d choose i) n^i (-1)^{d-i}. *)
    let transform d =
        let n = Var.of_string "n" in
        List.fold_right (fun i p ->
        let coeff =
            let exp = (d - i) mod 2 in
            OurInt.((pow (of_int (-1)) exp) * (binomial d i)) |> OurRational.of_ourint in
            let poly = match i with
            | 0 -> RationalPolynomial.of_constant coeff
            | _ -> RationalPolynomial.mult_with_const coeff (RationalPolynomial.of_power n i) in
            RationalPolynomial.add p poly) (List.range 0 `To d) (RationalPolynomial.zero)

    module ClosedFormTable = Hashtbl.Make(Var)

    let closed_form_table: t ClosedFormTable.t = ClosedFormTable.create 10

    (* c.f.: masterthesis *)
    let multiply var poly monomials =
            let scaled_monomials = Polynomial.scaled_monomials poly in
            List.fold_right (fun tmp pe -> add tmp pe)
                (List.map (fun scaled_monom ->
                    let vars = scaled_monom |> VarSet.to_list % ScaledMonomial.vars in
                    let monom = ScaledMonomial.monomial scaled_monom in
                    let constant = (mk_cons (OurRational.of_ourint (ScaledMonomial.coeff scaled_monom)), 1) in
                    vars
                    |> List.map (fun var -> (ClosedFormTable.find closed_form_table var, Monomial.degree_variable var monom))
                    |> List.append [constant]
                    |> power_list) scaled_monomials) []
        |> simplify

    (* c.f.: eq. (6) *)
    let only_poly var poly =
        let monomials = Polynomial.monomials poly in
        let pe = multiply var poly monomials in
        let pe_ = List.fold_right (fun (c, p, d, b) pe ->
            add (List.map (fun i -> (
                let coeff = let exp = (d - i) mod 2 in
                            let term = OurInt.((pow (of_int (-1)) exp) * (binomial d i)) |> OurRational.of_ourint in
                            OurRational.div  term (OurRational.of_int b) in
                ConstantConstraint.mk_and (ConstantConstraint.increase OurInt.one c) (ConstantConstraint.mk_neq OurInt.zero),
                RationalPolynomial.mult_with_const coeff p,
                i,
                b)) (List.range 0 `To d)) pe) pe [] in (** TODO: QQ[x] ?*)
        [(ConstantConstraint.mk_eq OurInt.zero, RationalPolynomial.of_var var, 0, 1)] @ pe_

    (* c.f.: eq. (7) *)
    let positive (c, p, d, b) pos_const coeff_var =
        if ConstantConstraint.is_false c then
            []
        else
            let pos_const_int = OurInt.to_int pos_const in
            let coeff = OurRational.div
                OurRational.(of_ourint OurInt.(mul (pow pos_const d) (pow (of_int b) pos_const_int)))
                OurRational.(of_ourint OurInt.(pow (of_int coeff_var) (to_int (pos_const + OurInt.one)))) in
            [(ConstantConstraint.mk_gt pos_const_int, RationalPolynomial.mult_with_const coeff p, 0, coeff_var)]


    let rec compute_r q c =
        let n = Var.of_string "n" in
        if RationalPolynomial.is_const q then
            if OurRational.equal OurRational.one c then
                RationalPolynomial.of_coeff_list [RationalPolynomial.get_constant q] [n]
            else
                RationalPolynomial.of_constant OurRational.(div (RationalPolynomial.get_constant q)  (one - c))
        else
            let d = RationalPolynomial.degree q in
            let c_d = q |> RationalPolynomial.degree_coeff_list |> List.last in
            if OurRational.equal OurRational.one c then
                let s = RationalPolynomial.mult_with_const (OurRational.div c_d (OurRational.of_int (d + 1))) (RationalPolynomial.of_power n (d + 1)) in
                let s_ = RationalPolynomial.mult_with_const (OurRational.div c_d (OurRational.of_int (d + 1))) (transform (d + 1)) in
                RationalPolynomial.add s (compute_r RationalPolynomial.(add (sub q s) (mult_with_const c s_)) c)
            else
                let s = RationalPolynomial.mult_with_const OurRational.(div c_d (one - c)) (RationalPolynomial.of_power n d) in
                let s_ = RationalPolynomial.mult_with_const OurRational.(div c_d (one - c)) (transform d) in
                RationalPolynomial.add s (compute_r RationalPolynomial.(add (sub q s) (mult_with_const c s_)) c)

    (* c.f.: eq. (8) *)
    let no_positive (c, p, d, b) max_const coeff_var =
        if ConstantConstraint.is_false c then
            []
        else
            (* c.f.: eq. (9) *)
            let rec le_max i =
                let tmp = i - 1 in
                let term =
                    if ConstantConstraint.eval (OurInt.of_int tmp) c && i >= 1 then
                    let coeff = OurRational.div
                        OurRational.(of_ourint OurInt.(mul (pow (of_int tmp) d) (pow (of_int b) tmp)))
                        OurRational.(of_ourint OurInt.(pow (of_int coeff_var) i)) in
                    [(ConstantConstraint.mk_gt tmp, RationalPolynomial.mult_with_const coeff p, 0, coeff_var)]
                    else [] in match i with
                | -1 -> []
                | 0 -> []
                | _ -> term@(le_max tmp) in
            (* c.f.: eq. (10) *)
            let gt_max =
                let r = compute_r (transform d) OurRational.(div (of_int coeff_var) (of_int b)) in
                let max_const_inc = OurInt.(max_const + one) in
                let coeff = OurRational.(mul (div (pow (div (of_int b) (of_int coeff_var)) (OurInt.to_int max_const_inc)) (of_int (-b)))
                                     (RationalPolynomial.eval_f r (fun _ -> OurRational.of_ourint max_const_inc))) in
                let coeff_list = RationalPolynomial.degree_coeff_list r in
                let degree_r = (List.length coeff_list) - 1 in
                let rec pe_ coeff_list_ i  =
                    let term x = [(ConstantConstraint.mk_gt ((OurInt.to_int max_const) + 1), RationalPolynomial.mult_with_const (OurRational.div x (OurRational.of_int b)) p, degree_r - i, b)] in
                    match coeff_list_ with
                        | [] -> []
                        | x::xs -> (term x)@(pe_ xs (i - 1)) in
                [(ConstantConstraint.mk_gt ((OurInt.to_int max_const) + 1), RationalPolynomial.mult_with_const coeff p, 0, coeff_var)]@(pe_ coeff_list degree_r) in
            (le_max ((OurInt.to_int max_const) + 1))@gt_max

    let poly_with_var var poly =
        let monomials = Polynomial.monomials poly |> List.filter (fun monomial -> (Monomial.degree_variable var monomial) = 0) in
        let coeff_var = poly |> Polynomial.coeff_of_var var |> OurInt.to_int in
        let pe = multiply var (Polynomial.sub poly (Polynomial.of_coeff_list [OurInt.of_int coeff_var] [var])) monomials in
        let pe_ = List.fold_right (fun (c, p, d, b) tmp -> let pos_const = ConstantConstraint.contains_positive c in
            if Option.is_some pos_const then
                    (positive (c, p, d, b) (Option.get pos_const) coeff_var) @ tmp
                else
                    (no_positive (c, p, d, b) (ConstantConstraint.max_constant c)  coeff_var) @ tmp ) pe [] in
        [(ConstantConstraint.mk_true, RationalPolynomial.of_var var, 0, coeff_var)] @ pe_

    let normalize pe_list =
        pe_list
        |>  List.map (fun pe ->
                List.map (fun (c, p, d, b) ->
                    if Option.is_some (ConstantConstraint.contains_positive c) then (ConstantConstraint.mk_false, p, d, b)
                    else (ConstantConstraint.mk_true, p, d, b)) pe
                |> simplify)

    let remove_frac pe =
        let lcm = List.map (RationalPolynomial.coeffs % Tuple4.second) pe |> List.concat |> List.map OurRational.den |> OurInt.lcm_list in
        List.map (fun (c,p,d,b) -> (c, RationalPolynomial.mult_with_const (OurRational.of_ourint lcm) p, d, b)) pe

    let max_const = function
    | [] -> OurInt.zero
    | xs -> OurInt.max_list (List.map (ConstantConstraint.max_constant % Tuple4.first) xs)

    (** We assume that we get a list of var -> poly, s.t., the list ordering corresponds to a valid twn order. *)
    let compute_closed_form var_poly =
        ClosedFormTable.clear closed_form_table;
        List.map (fun (var, poly) -> let tmp = if VarSet.mem var (Polynomial.vars poly) then poly_with_var var poly else only_poly var poly in
                                     tmp
                                     |> simplify
                                     |> tap (fun pe -> ClosedFormTable.add closed_form_table var pe)) var_poly


    (* Monotonic Kernel *)
    module SMTSolver = SMT.Z3Solver

    let red_gt poly_list =
        let rec constraint_eq_zero i = function
        | [] -> Constraint.mk_true
        | x::xs when i == 0 -> Constraint.mk_true
        | x::xs -> Constraint.(mk_and (mk_eq x Polynomial.zero) (constraint_eq_zero (i - 1) xs))  in
        let rec formula i = function
        | [] -> Formula.mk_false
        | x::xs -> Formula.(mk_or (mk_and (mk_gt x Polynomial.zero) (constraint_eq_zero (i - 1) poly_list |> Formula.mk)) (formula (i + 1) xs))  in
        formula 1 poly_list

    module Valuation = Valuation.Make(OurInt)

    let negative_dominated invariant guard npe =
    let formula = red_gt (List.map (RationalPolynomial.normalize % Tuple4.second) npe) in (* npe > 0 *)
    SMTSolver.satisfiable Formula.(mk_and (mk_and invariant guard) formula |> simplify) (* there exist a model sat. guard && inv && red(npe > 0) *)
    |> not

    module ScaledMonomialRational = ScaledMonomials.Make(OurRational)
    module MonomialRational = Monomials.Make(OurRational)

    let monotonic_kernel precondition guard = function
        | [] -> [], []
        | [x] -> [x], []
        | x::xs ->
        let f_matching_monomial scm = List.find_opt (fun (c,p,d,b) ->
            List.exists (fun scm2 ->
            OurRational.sign (ScaledMonomialRational.coeff scm) != (OurRational.sign (ScaledMonomialRational.coeff scm2)) &&
            MonomialRational.equal (ScaledMonomialRational.monomial scm) (ScaledMonomialRational.monomial scm2)) (RationalPolynomial.scaled_monomials p)) xs in
        let ys, mths = List.fold_right (fun (c0,p0,d0,b0) (tmp, mths) (* t1 *) ->
            let (c,p,d,b) = (c0,p0,d0,b0)::tmp |> simplify |> List.first
            and ys = (c0,p0,d0,b0)::tmp |> simplify |> List.tl in
            List.fold_right (fun scm (zs, mths_inner) ->
                if SMTSolver.tautology Formula.(implies precondition (mk_gt (RationalPolynomial.normalize (RationalPolynomial.of_scaled [scm])) Polynomial.zero)) then
                    (** positive |= pre -> mon > 0*)
                    let matching_monomial = f_matching_monomial scm in
                    if Option.is_some matching_monomial then
                        let (c1,p1,d1,b1) = Option.get matching_monomial in
                        ((c1, [scm |> ScaledMonomialRational.mult_with_const OurRational.minus_one] |> RationalPolynomial.of_scaled,d1,b1)::zs |> simplify, ((b1,d1),(b,d))::mths_inner)
                    else
                        ((c,RationalPolynomial.of_scaled [scm],d,b)::zs, mths_inner)
                else if SMTSolver.tautology Formula.(implies precondition (mk_le (RationalPolynomial.normalize (RationalPolynomial.of_scaled [scm])) Polynomial.zero)) then
                    (** negative |= pre -> mon <= 0 *)
                    (zs, ((b,d),(0,0))::mths_inner)
                else
                    ((c,RationalPolynomial.of_scaled [scm],d,b)::zs), mths_inner) (RationalPolynomial.scaled_monomials p) (ys, mths)) xs ([],[]) in
        if negative_dominated precondition guard (x::ys) then
            (x::ys |> simplify), mths
        else
            x::xs, []

    open BoundsInst

    let overapprox t runtime_bound =
        List.map (fun (c, p, d, b) ->
            let bound_p = p |> RationalPolynomial.overapprox |> Bound.of_poly
            and bound_d = Bound.pow runtime_bound d
            and bound_b = Bound.exp (OurInt.of_int b) runtime_bound in Bound.(add (add bound_p bound_d) bound_b)) t
        |> Bound.sum_list
end
