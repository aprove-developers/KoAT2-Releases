open Atoms
open Batteries
open Formulas
open Polynomials
open PolyExponential

let logger = Logging.(get Twn)

module Make (Bound : BoundType.Bound) (PM : ProgramTypes.ClassicalProgramModules) = struct
  module Check_TWN = Check_TWN.Make (Bound) (PM)
  module Loop = Loop.Make (Bound) (PM)
  module TWN_Termination = TWN_Termination.Make (Bound) (PM)

  (* COMPLEXITY: *)

  (* Computes the monotonicity threshold for (b1,a1) > (b2,a2), i.e., smallest m s.t for all n >= m: n^a1 * b1^n > k * n^a1 * b1^n *)
  let monotonicity_th k (b1, a1) (b2, a2) =
    if OurInt.is_negative k || OurInt.is_zero k then
      OurInt.one
    else if OurInt.is_zero a1 && (not @@ OurInt.is_zero a2) then
      OurInt.zero
    else if OurInt.is_zero a1 && OurInt.is_zero a2 then
      OurRational.(ceil @@ (log (of_ourint k) + one))
    else
      let rec test_m m =
        let tmp1 = OurRational.((of_ourint @@ OurInt.pow_ourint m a1) * pow_ourint b1 m) in
        let tmp2 = OurRational.((of_ourint @@ OurInt.pow_ourint m a2) * pow_ourint b2 m * of_ourint k) in
        if OurInt.is_ge a1 a2 then
          if OurRational.(tmp1 > tmp2) then
            m
          else
            test_m OurInt.(add m one)
        else
          let tmp =
            OurRational.(mul (pow_ourint (make m OurInt.(add m one)) (OurInt.sub a2 a1)) (div b1 b2))
          in
          if OurRational.(tmp >= one && tmp1 > tmp2) then
            m
          else
            test_m OurInt.(add m one)
      in
      let rec decrease_m m =
        if OurInt.is_zero m then
          m
        else
          let m_dec = OurInt.(m - one) in
          let tmp1 = OurRational.((of_ourint @@ OurInt.pow_ourint m_dec a1) * pow_ourint b1 m_dec) in
          let tmp2 =
            OurRational.((of_ourint @@ OurInt.pow_ourint m_dec a2) * pow_ourint b2 m_dec * of_ourint k)
          in
          if OurRational.(tmp2 >= tmp1) then
            m
          else
            decrease_m m_dec
      in
      OurInt.zero |> test_m |> decrease_m


  let monotonicity_th_int k (b1, a1) (b2, a2) =
    monotonicity_th (OurInt.of_int k)
      (OurRational.of_int b1, OurInt.of_int a1)
      (OurRational.of_int b2, OurInt.of_int a2)


  (* let compute_kmax sub_poly = sub_poly |> List.map (OurInt.max_list % (List.map OurInt.abs) % Polynomial.coeffs) |> OurInt.max_list *)

  let compute_N = function
    | [] -> OurInt.zero
    | x :: [] -> OurInt.zero
    | [ x; y ] -> OurInt.one
    | x1 :: x2 :: x3 :: xs ->
        let mt = List.map (fun x -> monotonicity_th OurInt.one x3 x) xs |> OurInt.max_list in
        let mt_ = monotonicity_th (OurInt.of_int (List.length xs + 1)) x2 x3 in
        OurInt.max mt mt_


  let compute_M = function
    | [] -> OurInt.zero
    | x :: [] -> OurInt.zero
    | (b1, a1) :: (b2, a2) :: _ when OurRational.(b1 > b2 || (b1 == b2 && OurInt.(a1 > a2 + one))) ->
        monotonicity_th OurInt.one (b1, a1) (b2, OurInt.(a2 + one))
    | _ -> OurInt.zero


  let compute_M' eps = function
    | [] -> OurInt.zero
    | x :: [] -> OurInt.zero
    | (b1, a1) :: (b2, a2) :: _ -> monotonicity_th OurInt.one (OurRational.(b2 + eps), a1) (b2, a2)


  module ScaledMonomial = ScaledMonomials.Make (OurInt)
  module Monomial = Monomials.Make (OurInt)

  let compute_alpha_abs = function
    | [] -> Polynomial.zero
    | x :: [] ->
        Polynomial.fold
          ~const:(Polynomial.of_constant % OurInt.abs)
          ~indeterminate:Polynomial.of_var ~plus:Polynomial.add ~times:Polynomial.mul ~pow:Polynomial.pow x
    | xs ->
        List.flatten (List.map Polynomial.scaled_monomials xs)
        |> List.group (fun x y -> Monomial.compare (ScaledMonomial.monomial x) (ScaledMonomial.monomial y))
        |> List.map (fun ys ->
               let max = OurInt.max_list (List.map (OurInt.abs % ScaledMonomial.coeff) ys) in
               List.first ys |> ScaledMonomial.monomial |> ScaledMonomial.make max)
        |> Polynomial.make


  let compute_f twn_proofs atom = function
    | [] -> Bound.zero
    | x :: [] -> Bound.one
    | xs ->
        let alphas = List.map Tuple4.second xs |> List.tl in
        Logger.log logger Logger.INFO (fun () ->
            ( "complexity.compute_f",
              [ ("alphas", alphas |> List.enum |> Util.enum_to_string Polynomial.to_string) ] ));
        let alphas_abs = compute_alpha_abs alphas in
        Logger.log logger Logger.INFO (fun () ->
            ("complexity.compute_f", [ ("alphas_abs", Polynomial.to_string alphas_abs) ]));
        let base_exp ys = List.map (fun (_, _, d, b) -> (OurRational.of_ourint b, d)) ys in
        let prefix ys = List.fold_left (fun acc itt -> (List.hd acc @ [ itt ]) :: acc) [ [] ] ys in
        let n_ = OurInt.max_list (List.map (fun ys -> compute_N (base_exp ys)) (prefix xs)) in
        Logger.log logger Logger.INFO (fun () -> ("complexity.compute_f", [ ("N", OurInt.to_string n_) ]));
        let m_ = OurInt.max_list (List.map (fun ys -> compute_M (base_exp ys)) (prefix xs)) in
        Logger.log logger Logger.INFO (fun () -> ("complexity.compute_f", [ ("M", OurInt.to_string m_) ]));
        Bound.of_intpoly
          Polynomial.(alphas_abs + alphas_abs + (of_constant @@ OurInt.max m_ n_) + Polynomial.one)
        |> tap (fun b ->
               Logger.log logger Logger.INFO (fun () ->
                   ("complexity.compute_f", [ ("2*alpha_abs+max(N,M)", Bound.to_string b) ]));
               ProofOutput.LocalProofOutput.add_to_proof twn_proofs (fun () ->
                   [
                     "alphas_abs: " ^ Polynomial.to_string_pretty alphas_abs;
                     "M: " ^ OurInt.to_string m_;
                     "N: " ^ OurInt.to_string n_;
                     "Bound: " ^ Bound.to_string ~pretty:true b;
                   ]
                   |> List.map FormattedString.mk_str_line |> FormattedString.mappend
                   |> FormattedString.mk_block
                   |> FormattedString.( <> )
                        (FormattedString.mk_str_line
                           ("Stabilization-Threshold for: " ^ Atom.to_string ~pretty:true atom))))


  let rec is_sorted = function
    | []
    | [ _ ] ->
        true
    | x1 :: x2 :: xs -> OurInt.(x1 > x2) && is_sorted (x2 :: xs)


  let rec min_logbase eps = function
    | []
    | [ _ ] ->
        OurRational.one
    | x1 :: x2 :: xs -> OurRational.(min (x1 / (x2 + eps)) (min_logbase eps (x2 :: xs)))


  let compute_f' twn_proofs atom = function
    | [] -> Bound.zero
    | x :: [] -> Bound.one
    | xs ->
        let alphas = List.map Tuple4.second xs |> List.tl in
        Logger.log logger Logger.INFO (fun () ->
            ( "complexity.compute_f'",
              [ ("alphas", alphas |> List.enum |> Util.enum_to_string Polynomial.to_string) ] ));
        let alphas_abs = compute_alpha_abs alphas in
        Logger.log logger Logger.INFO (fun () ->
            ("complexity.compute_f'", [ ("alphas_abs", Polynomial.to_string alphas_abs) ]));
        let base_exp ys = List.map (fun (_, _, d, b) -> (OurRational.of_ourint b, d)) ys in
        let prefix ys = List.fold_left (fun acc itt -> (List.hd acc @ [ itt ]) :: acc) [ [] ] ys in
        let n_ = OurInt.max_list (List.map (fun ys -> compute_N (base_exp ys)) (prefix xs)) in
        Logger.log logger Logger.INFO (fun () -> ("complexity.compute_f'", [ ("N", OurInt.to_string n_) ]));
        let eps = OurRational.(one / of_int 2) in
        let m'_ = OurInt.max_list (List.map (fun ys -> compute_M' eps (base_exp ys)) (prefix xs)) in
        Logger.log logger Logger.INFO (fun () -> ("complexity.compute_f'", [ ("M", OurInt.to_string m'_) ]));
        let coeff =
          OurRational.(ceil @@ (one / min_logbase eps (List.map (OurRational.of_ourint % Tuple4.fourth) xs)))
        in
        Bound.(
          (of_OurInt coeff * log_of_poly Polynomial.(alphas_abs + alphas_abs))
          + (of_OurInt @@ OurInt.max m'_ n_))
        |> tap (fun b ->
               Logger.log logger Logger.INFO (fun () ->
                   ("complexity.compute_f'", [ ("log(2*alpha_abs)+max(N,M')", Bound.to_string b) ]));
               ProofOutput.LocalProofOutput.add_to_proof twn_proofs (fun () ->
                   [
                     "alphas_abs: " ^ Polynomial.to_string_pretty alphas_abs;
                     "M': " ^ OurInt.to_string m'_;
                     "N: " ^ OurInt.to_string n_;
                     "Bound: " ^ Bound.to_string ~pretty:true b;
                   ]
                   |> List.map FormattedString.mk_str_line |> FormattedString.mappend
                   |> FormattedString.mk_block
                   |> FormattedString.( <> )
                        (FormattedString.mk_str_line
                           ("Stabilization-Threshold for: " ^ Atom.to_string ~pretty:true atom))))


  let get_bound twn_proofs ((guard, update) : Loop.t) order npe varmap =
    let bound, max_con =
      List.fold_right
        (fun atom (bound, const) ->
          let poly =
            (* Transform from p ≤ 0 to p < 0 *)
            Atom.poly_lt atom |> Polynomial.neg
          in
          let sub_poly =
            RationalPE.substitute varmap (RationalPolynomial.of_intpoly poly) |> RationalPE.remove_frac
          in
          Logger.log logger Logger.INFO (fun () ->
              ( "complexity: npe -> guard_atom",
                [ ("atom", Atom.to_string atom); ("subs", "0 <= " ^ RationalPE.to_string sub_poly) ] ));
          let sub_poly_n =
            sub_poly
            |> List.map (fun (c, p, d, b) ->
                   (c, RationalPolynomial.normalize p, d |> OurInt.of_int, OurRational.to_ourint b))
          in
          let max_const = OurInt.max const (RationalPE.max_const sub_poly) in
          if is_sorted @@ List.map Tuple4.fourth sub_poly_n then
            (Bound.add bound (compute_f' twn_proofs atom sub_poly_n), max_const)
          else
            (Bound.add bound (compute_f twn_proofs atom sub_poly_n), max_const))
        (guard |> Formula.atoms |> List.unique ~eq:Atom.equal)
        (Bound.one, OurInt.zero)
    in
    (* TODO guard without inv *)
    Logger.log logger Logger.INFO (fun () ->
        ("complexity.get_bound", [ ("max constant in constant constraint", OurInt.to_string max_con) ]));
    Bound.(add bound (of_OurInt OurInt.(max_con + one)))
    |> tap (fun b ->
           Logger.log logger Logger.INFO (fun () ->
               ("complexity.get_bound", [ ("local bound", Bound.to_string b) ])))


  let complexity twn_proofs ?(entry = None) ?(termination = true) ((guard, update) : Loop.t) =
    let loop = (guard, update) in
    let order = Check_TWN.(unwrap_twn @@ check_triangular loop) in
    let t_, was_negative =
      if Check_TWN.check_weakly_negativitiy loop then
        ( Loop.chain loop
          |> tap (fun loop ->
                 Logger.log logger Logger.INFO (fun () -> ("negative", [ ("chained", Loop.to_string loop) ]))),
          true )
      else
        (loop, false)
    in
    Logger.log logger Logger.INFO (fun () ->
        ("order", [ ("order", Util.enum_to_string Var.to_string (List.enum order)) ]));
    ProofOutput.LocalProofOutput.add_to_proof twn_proofs (fun () ->
        FormattedString.mk_str_line ("  loop: " ^ Loop.to_string (guard, update)));
    ProofOutput.LocalProofOutput.add_to_proof twn_proofs (fun () ->
        FormattedString.mk_str_line
          ("  order: " ^ Util.enum_to_string (Var.to_string ~pretty:true) (List.enum order)));
    let pe =
      RationalPE.compute_closed_form
        (List.map
           (fun var ->
             let update_var = Loop.update_var t_ var in
             (var, RationalPolynomial.of_intpoly update_var))
           order)
    in
    Logger.log logger Logger.INFO (fun () ->
        ("closed-form", List.combine (List.map Var.to_string order) (List.map RationalPE.to_string pe)));
    ProofOutput.LocalProofOutput.add_to_proof twn_proofs (fun () ->
        FormattedString.(
          mk_str "closed-form:"
          <> (List.combine
                (List.map (Var.to_string ~pretty:true) order)
                (List.map RationalPE.to_string_pretty pe)
             |> List.map (fun (a, b) -> a ^ ": " ^ b)
             |> List.map FormattedString.mk_str_line |> FormattedString.mappend |> FormattedString.mk_block)));
    let npe = RationalPE.normalize pe in
    Logger.log logger Logger.INFO (fun () ->
        ( "constrained-free closed-form",
          List.combine (List.map Var.to_string order) (List.map RationalPE.to_string npe) ));
    let varmap = Hashtbl.of_list @@ List.combine order npe in
    let terminating =
      if not termination then
        true
      else
        TWN_Termination.termination_ twn_proofs t_ ~entry varmap
    in
    if not terminating then
      Bound.infinity
    else
      let f = get_bound twn_proofs t_ order npe varmap in
      if was_negative then
        Bound.(f + f + one)
      else
        f


  let complexity_ twn_proofs t =
    if Check_TWN.check_twn_t t then
      complexity twn_proofs @@ (Loop.mk % Tuple3.second) t
    else
      Bound.infinity
end
