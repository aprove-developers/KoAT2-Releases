open Atoms
open Batteries
open BoundsInst
open Formulas
open Polynomials
open PolyExponential

let logger = Logging.(get Twn)

module Make(PM: ProgramTypes.ClassicalProgramModules) = struct
  module Check_TWN = Check_TWN.Make(PM)
  module Loop = Loop.Make(PM)
  module TWN_Termination = TWN_Termination.Make(PM)

  (* COMPLEXITY: *)

  (* Computes the monotonicity threshold for (b1,a1) > (b2,a2), i.e., smallest m s.t for all n >= m: n^a1 * b1^n > k * n^a1 * b1^n *)
  let monotonicity_th k (b1,a1) (b2,a2) =
    if OurInt.is_negative k || OurInt.is_zero k then OurInt.one
    else
      let rec test_m m =
        let tmp1 = OurInt.((pow_ourint m a1) * (pow_ourint b1 m)) in
        let tmp2 = OurInt.((pow_ourint m a2) * (pow_ourint b2 m) * k) in
        if OurInt.is_ge a1 a2 then
          if (OurInt.is_gt tmp1 tmp2) then m else test_m OurInt.(add m one)
        else
          let tmp = OurRational.(mul (pow_ourint (reduce (m , OurInt.(add m one))) (OurInt.sub a2 a1)) (reduce (b1, b2))) in
          if (OurRational.(is_ge tmp one) && (OurInt.is_gt tmp1 tmp2)) then m else test_m OurInt.(add m one) in
      let rec decrease_m m =
        if OurInt.is_zero m then m
        else
          let m_dec = OurInt.(m - one) in
          let tmp1 = OurInt.((pow_ourint m_dec a1) * (pow_ourint b1 m_dec)) in
          let tmp2 = OurInt.((pow_ourint m_dec a2) * (pow_ourint b2 m_dec) * k) in
          if OurInt.is_ge tmp2 tmp1 then m else decrease_m m_dec in
      OurInt.zero |> test_m |> decrease_m

  let monotonicity_th_int k (b1,a1) (b2,a2) = monotonicity_th (OurInt.of_int k) (OurInt.of_int b1, OurInt.of_int a1) (OurInt.of_int b2, OurInt.of_int a2)

  (* let compute_kmax sub_poly = sub_poly |> List.map (OurInt.max_list % (List.map OurInt.abs) % Polynomial.coeffs) |> OurInt.max_list *)

  let compute_N = function
    | [] -> OurInt.zero
    | x::[] -> OurInt.zero
    | x::y::[] -> OurInt.one
    | x1::x2::x3::xs ->
      let mt = List.map (fun x -> monotonicity_th OurInt.one x3 x) xs |> OurInt.max_list in
      let mt_ = monotonicity_th (OurInt.of_int ((List.length xs) + 1)) x2 x3 in
      OurInt.max mt mt_

  let compute_M = function
    | [] -> OurInt.zero
    | x::[] -> OurInt.zero
    | (b1,a1)::(b2,a2)::xs when ((OurInt.is_gt b1 b2) || ((OurInt.equal b1 b2) && (OurInt.is_gt a1 OurInt.(add a2 one))))
                           -> monotonicity_th OurInt.one (b1,a1) (b2, OurInt.(add a2 one))
    | _ -> OurInt.zero

  module ScaledMonomial = ScaledMonomials.Make(OurInt)
  module Monomial = Monomials.Make(OurInt)

  let compute_alpha_abs = function
    | [] -> Polynomial.zero
    | x::[] -> Polynomial.fold ~const:(Polynomial.of_constant % OurInt.abs) ~indeterminate:(Polynomial.of_var) ~neg:identity ~plus:Polynomial.add ~times:Polynomial.mul ~pow:Polynomial.pow x
    | xs -> List.flatten (List.map Polynomial.scaled_monomials xs)
            |> List.group (fun x y -> Monomial.compare (ScaledMonomial.monomial x) (ScaledMonomial.monomial y))
            |> List.map (fun ys -> let max = OurInt.max_list (List.map (OurInt.abs % ScaledMonomial.coeff) ys) in
                        List.first ys |> ScaledMonomial.monomial |> ScaledMonomial.make max)
            |> Polynomial.of_scaled

  let compute_f atom = function
    | [] -> Bound.zero
    | x::[] -> Bound.one
    | xs ->
      let alphas = List.map (Tuple4.second) xs |> List.tl in
      Logger.log logger Logger.INFO (fun () -> "complexity.compute_f", ["alphas", (alphas |> List.enum |> Util.enum_to_string Polynomial.to_string)]);
      let alphas_abs = compute_alpha_abs alphas in
      Logger.log logger Logger.INFO (fun () -> "complexity.compute_f", ["alphas_abs", Polynomial.to_string alphas_abs]);
      let base_exp ys = List.map (fun (_,_,d,b) -> (b,d)) ys in
      let prefix ys = List.fold_left (fun acc itt -> ((List.hd acc)@[itt])::acc) [[]] ys in
      let n_ = OurInt.max_list (List.map (fun ys -> compute_N (base_exp ys)) (prefix xs)) in
      Logger.log logger Logger.INFO (fun () -> "complexity.compute_f", ["N", (OurInt.to_string n_)]);
      let m_ = OurInt.max_list (List.map (fun ys -> compute_M (base_exp ys)) (prefix xs)) in
      Logger.log logger Logger.INFO (fun () -> "complexity.compute_f", ["M", (OurInt.to_string m_)]);
      Bound.(of_poly (Polynomial.add alphas_abs alphas_abs) |> add (of_constant (OurInt.max m_ n_)) |> add (of_constant OurInt.one))
      |> tap (fun b ->
        Logger.log logger Logger.INFO (fun () -> "complexity.compute_f", ["2*alpha_abs+max(N,M)", Bound.to_string b]);
        TWN_Proofs.proof_append (
        [ "alphas_abs: " ^ (Polynomial.to_string_pretty alphas_abs);
          "M: " ^ (OurInt.to_string m_);
          "N: " ^ (OurInt.to_string n_);
          "Bound: " ^ (Bound.to_string ~pretty:true b);
        ] |> List.map (FormattedString.mk_str_line) |> FormattedString.mappend |> FormattedString.mk_block |> FormattedString.(<>) (FormattedString.mk_str_line ("Stabilization-Threshold for: " ^ (Atom.to_string ~pretty:true atom))));)

  let get_bound ((guard,update): Loop.t) order npe varmap =
    let bound, max_con =
      List.fold_right (fun atom (bound, const) ->
        let poly = Atom.poly atom |> Polynomial.neg in
        let sub_poly = PE.substitute varmap poly |> PE.remove_frac (* TODO |> PE.monotonic_kernel inv (TWNLoop.guard t) *) in
        Logger.log logger Logger.INFO (fun () -> "complexity: npe -> guard_atom", ["atom", Atom.to_string atom; "subs", "0 <= " ^ PE.to_string sub_poly]);
        let sub_poly_n = sub_poly |> List.map (fun (c,p,d,b) -> (c, RationalPolynomial.normalize p , d |> OurInt.of_int, b |> OurInt.of_int)) in
        let max_const = OurInt.max const (PE.max_const sub_poly) in
          Bound.add bound (compute_f atom sub_poly_n), max_const)
          (guard |> Formula.atoms |> List.unique ~eq:Atom.equal) (Bound.one, OurInt.zero) in (* TODO guard without inv *)
          Logger.log logger Logger.INFO (fun () -> "complexity.get_bound", ["max constant in constant constraint", OurInt.to_string max_con]);
    Bound.(add bound (of_constant (OurInt.add max_con OurInt.one)))
    |> tap (fun b -> Logger.log logger Logger.INFO (fun () -> "complexity.get_bound", ["local bound", Bound.to_string b]))

  let complexity ?(entry = None) ?(termination = true) upd_invariant_cand ((guard,update): Loop.t) =
    let loop = (guard,update) in
    let order = Check_TWN.check_triangular loop in
    let t_, was_negative =
      if (Check_TWN.check_weakly_negativitiy loop) then
        Loop.chain loop |> tap (fun loop -> Logger.log logger Logger.INFO (fun () -> "negative", ["chained", Loop.to_string loop])), true
      else loop, false in
    Logger.log logger Logger.INFO (fun () -> "order", ["order", Util.enum_to_string Var.to_string (List.enum order)]);
    TWN_Proofs.proof_append (FormattedString.mk_str_line ("  loop: " ^ Loop.to_string (guard,update)));
    TWN_Proofs.proof_append (FormattedString.mk_str_line ("  order: " ^ (Util.enum_to_string (Var.to_string ~pretty:true) (List.enum order))));
    let pe = PE.compute_closed_form (List.map (fun var ->
      let update_var = Loop.update_var t_ var in
      var, update_var) order) in
      Logger.log logger Logger.INFO (fun () -> "closed-form", (List.combine (List.map Var.to_string order) (List.map PE.to_string pe)));
      TWN_Proofs.proof_append (
        FormattedString.(mk_str "closed-form:" <> (
        (List.combine (List.map (Var.to_string ~pretty:true) order) (List.map PE.to_string_pretty pe))
        |> List.map (fun (a,b) -> a ^ ": " ^ b)
        |> List.map (FormattedString.mk_str_line) |> FormattedString.mappend |> FormattedString.mk_block)));
    let npe = PE.normalize pe in
      Logger.log logger Logger.INFO (fun () -> "constrained-free closed-form", List.combine (List.map Var.to_string order) (List.map PE.to_string npe));
    let varmap = Hashtbl.of_list @@ List.combine order npe in
    let terminating = if not termination then true else TWN_Termination.termination_ t_ ~entry:entry upd_invariant_cand varmap in
    if not terminating then
      Bound.infinity
    else
      let f = get_bound t_ order npe varmap in if was_negative then Bound.(f + f + one) else f

  let complexity_ (_,t,_) = complexity [] (Loop.mk t)
end
