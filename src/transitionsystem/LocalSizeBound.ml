open! OurBase
open Bounds
open Formulas
open Polynomials
open Constraints

let logger = Logging.(get LocalSizeBound)

module Solver = SMT.IncrementalZ3Solver

(** Performs a binary search between the lowest and highest value to find the optimal value which satisfies the predicate.
    We assume that the highest value already satisfies the predicate.
    Therefore this method always finds a solution. *)
let rec binary_search ?(divisor = 2.) (lowest : int) (highest : int) (p : int -> bool) =
  if lowest >= highest then
    lowest
  else
    (* We need to ensure that the result is always round down to prevent endless loops.
       Normal integer division rounds towards zero. *)
    let newBound = Float.to_int @@ Float.round_down @@ (Float.of_int (lowest + highest) /. divisor) in
    if p newBound then
      binary_search
        ~divisor:
          (if newBound < 0 then
             2.
           else
             divisor)
        lowest newBound p
    else
      binary_search
        ~divisor:
          (if newBound < 0 then
             divisor
           else
             2.)
        (newBound + 1) highest p


(* For 's' it is sufficient to only view the max occurring constants of the update polynomial. *)
let s_range update =
  update |> Polynomial.max_of_occurring_constants
  |> OurInt.max (OurInt.of_int 1) (* 0 or lower is not allowed *)
  |> OurInt.min (OurInt.of_int 1024)
     (* TODO We cut it at the moment at 1024, because sometimes the approximation is worse than an integer value. *)
  |> OurInt.to_int


(* For 'c' we want to view the max occurring constants of the complete formula *)
let c_range formula =
  formula |> Formula.max_of_occurring_constants
  |> OurInt.min (OurInt.of_int 1024)
     (* TODO We cut it at the moment at 1024, because sometimes the approximation is worse than an integer value. *)
  |> OurInt.to_int


module Make
    (TL : ProgramTypes.ClassicalTransitionLabel)
    (T : ProgramTypes.Transition with type transition_label = TL.t)
    (RV : ProgramTypes.RV with type transition = T.t)
    (P : ProgramTypes.Program with type transition_label = TL.t) =
struct
  type t = { factor : Polynomial.t; constant : int; vars : VarSet.t } [@@deriving eq]

  type t_rec = { factor_rec : Polynomial.t; constant_rec : int; vars_rec : VarFunctionCallSet.t }
  [@@deriving eq]

  let mk ?(s = 1) ?(c = 0) vars =
    { factor = Polynomial.of_int @@ abs s; constant = abs c; vars = VarSet.of_string_list vars }


  let initial_lsb s c vs = { factor = Polynomial.of_int s; constant = c; vars = vs }
  let factor t = t.factor
  let factor_rec t = t.factor_rec
  let constant t = t.constant
  let constant_rec t = t.constant_rec
  let vars t = t.vars
  let vars_rec t = t.vars_rec
  let is_constant = Set.is_empty % vars

  let from_t_to_trec t =
    { factor_rec = t.factor; constant_rec = t.constant; vars_rec = VarFunctionCallSet.of_varset t.vars }


  let to_string lsb =
    "{" ^ "factor: " ^ Polynomial.to_string lsb.factor ^ "; " ^ "constant: " ^ Int.to_string lsb.constant
    ^ "; " ^ "vars: " ^ VarSet.to_string lsb.vars ^ "; " ^ "}"


  let to_string_rec lsb =
    "{" ^ "factor: " ^ Polynomial.to_string lsb.factor_rec ^ "; " ^ "constant: "
    ^ Int.to_string lsb.constant_rec ^ "; " ^ "vars: "
    ^ VarFunctionCallSet.to_string lsb.vars_rec
    ^ "; " ^ "}"


  let to_string_option = function
    | None -> "Unbounded"
    | Some lsb -> to_string lsb


  let to_string_option_tuple = function
    | None -> "Unbounded"
    | Some (lsb, b) -> to_string_rec lsb ^ " equality: " ^ Bool.to_string (Lazy.force b)


  let as_bound lsb =
    let vars_sum = Bound.sum @@ Sequence.map ~f:Bound.of_var (Set.to_sequence lsb.vars) in
    Bound.(of_poly lsb.factor * (of_int lsb.constant + vars_sum))


  let as_poly lsb =
    let vars_sum =
      PolyFunctionCall.(sum @@ Sequence.map ~f:of_function_call (Set.to_sequence lsb.vars_rec))
    in
    PolyFunctionCall.(of_poly lsb.factor_rec * (of_int lsb.constant_rec + vars_sum))


  let option_lsb_as_bound = function
    | Some a -> as_bound a
    | None -> Bound.infinity


  let option_lsb_as_bound_rec = function
    | Some a -> Bound.of_poly @@ PolyFunctionCall.to_poly @@ as_poly a
    | None -> Bound.infinity


  let is_bounded_with solver update_formula v' t =
    (* Prove that under formula the bound from validity_as_bound always evaluates to a non-negative value *)
    Solver.push solver;
    (* Check if as_bound is always greator or equal than v' *)
    Solver.add_bound_comparison solver `LT (as_bound t) (Bound.of_var v');
    let result = Solver.unsatisfiable solver in
    Solver.pop solver;
    result


  let is_of_equality_type t update_formula v' =
    (* Trivially holds for constant lsbs *)
    if Set.is_empty t.vars then
      true
    else if
      (* Trivially does not hold if scaling > 1 and variables are present *)
      (not Polynomial.(equal one t.factor || equal one t.factor)) && not (Set.is_empty t.vars)
    then
      false
    else if
      (* Trivially holds for identity lsbs *)
      Set.length t.vars = 1 && Int.equal 0 t.constant
    then
      true
    else if Formula.is_linear update_formula then (
      let solver = Solver.create ~model:false () in
      (* Find contra *)
      Solver.add solver update_formula;
      Set.to_list t.vars
      |> List.iter ~f:(fun v -> Solver.add_bound_comparison solver `LT (Bound.of_var v) (Bound.of_var v'));
      Solver.add_bound_comparison solver `LT (Bound.of_int t.constant) (Bound.of_var v');
      let contra_exists = Solver.satisfiable solver in
      not contra_exists)
    else
      false


  let optimize_s max_s predicate lsb =
    let s_result =
      binary_search ~divisor:16. 1 max_s (fun next_s ->
          predicate { lsb with factor = Polynomial.of_int next_s })
    in
    { lsb with factor = Polynomial.of_int s_result }


  let optimize_c max_c predicate lsb =
    let c_result =
      binary_search ~divisor:16. 0 max_c (fun next_c -> predicate { lsb with constant = next_c })
    in
    { lsb with constant = c_result }


  let find_bound update_vars v' update_formula max_s =
    let max_c = c_range update_formula in
    let execute () =
      let solver = Solver.create ~model:false () in
      Solver.add solver update_formula;
      let is_bounded b = is_bounded_with solver update_formula v' b in
      VarSet.powerset update_vars
      |> Sequence.map ~f:(initial_lsb max_s max_c)
      |> Sequence.filter ~f:is_bounded
      |> Sequence.map ~f:(optimize_s max_s is_bounded)
      |> Sequence.map ~f:(optimize_c max_c is_bounded)
      |> Sequence.hd
      |> Option.map ~f:(fun t -> (t, Lazy.from_fun (fun () -> is_of_equality_type t update_formula v')))
    in
    Logger.with_log logger Logger.DEBUG
      (fun () ->
        ( "find_bound",
          [
            ("update_vars", VarSet.to_string update_vars);
            ("v'", Var.to_string v');
            ("max_s", Int.to_string max_s);
            ("max_c", Int.to_string max_c);
            ("update_formula", Formula.to_string update_formula);
          ] ))
      ~result:(to_string_option % Option.map ~f:Tuple2.first)
      execute


  let from_update_poly program_vars update_var update =
    let module Monomial = Monomials.Make (OurInt) in
    let open OptionMonad in
    let to_abs_int = OurInt.to_int % OurInt.abs in
    let* const, factor, vars =
      try
        Polynomial.monomials_with_coeffs update
        |> List.fold_left
             ~f:(fun lsb (coeff, mon) ->
               let* const, factor, vars = lsb in
               match Sequence.to_list (Monomial.to_sequence mon) with
               | [] -> Option.return (const + to_abs_int coeff, factor, vars)
               | [ (v, 1) ] when Set.mem program_vars v ->
                   Option.return (const, max factor (to_abs_int coeff), Set.add vars v)
               | _ -> None)
             ~init:(Some (0, 1, VarSet.empty))
      with
      | OurInt.Overflow -> None
    in
    let lsb =
      {
        factor = Polynomial.of_int factor;
        vars;
        constant =
          (if const mod factor = 0 then
             const / factor
           else
             (const / factor) + 1);
      }
    in
    let is_equality_type = Polynomial.equal (Polynomial.of_var update_var) update in
    Option.return (lsb, Lazy.from_val is_equality_type)


  let from_update_polyrec program_vars update_var update =
    let module Monomial = Monomials.MakeOverIndeterminate (VarFunctionCall) (OurInt) in
    let open OptionMonad in
    let to_abs_int = OurInt.to_int % OurInt.abs in
    let* const, factor_rec, factor_poly, vars_rec =
      try
        PolyFunctionCall.monomials_with_coeffs update
        |> List.fold_left
             ~f:(fun lsb (coeff, mon) ->
               let* const, factor, factor_poly, vars = lsb in
               match Sequence.to_list (Monomial.to_sequence mon) with
               | [] -> Option.return (const + to_abs_int coeff, factor, factor_poly, vars)
               | [ (v, 1) ] when VarFunctionCall.is_integral v || VarFunctionCall.is_function_call v ->
                   Option.return (const, max factor (to_abs_int coeff), factor_poly, Set.add vars v)
               | [ (v, 1); (v', 1) ] when VarFunctionCall.is_integral v && VarFunctionCall.is_function_call v'
                 ->
                   Option.return
                     ( const,
                       max factor (to_abs_int coeff),
                       Polynomial.(add factor_poly (of_var (VarFunctionCall.to_var v))),
                       Set.add vars v' )
                   (* TODO use Polynomial.max instead of Polynomial.add *)
               | _ -> None)
             ~init:(Some (0, 1, Polynomial.zero, VarFunctionCallSet.empty))
      with
      | OurInt.Overflow -> None
    in
    let lsb =
      {
        factor_rec = Polynomial.(of_int factor_rec + factor_poly);
        (* TODO use Polynomial.max instead of Polynomial.+ *)
        vars_rec;
        constant_rec =
          (if const mod factor_rec = 0 then
             const / factor_rec
           else
             (const / factor_rec) + 1);
      }
    in
    let is_equality_type = PolyFunctionCall.equal (PolyFunctionCall.of_var update_var) update in
    Option.return (lsb, Lazy.from_val is_equality_type)


  let compute_bound program_vars m var =
    let execute () =
      let open OptionMonad in
      if RV.has_transition (m, var) then
        let t = T.label @@ RV.transition_ m in
        let* update = TL.update t var in
        if
          Set.are_disjoint (PolyFunctionCall.vars update) (Guard.vars @@ TL.guard t)
          && Set.is_subset (PolyFunctionCall.vars update) ~of_:program_vars
          || PolyFunctionCall.has_function_calls update
        then
          from_update_polyrec program_vars var update
        else
          let v' = Var.fresh_id Var.Int () in
          let update_formula =
            (* Facilitate SMT call by removing non-linear constraints. *)
            (* The resulting update_formula is an overapproximation of the original formula *)
            Formula.mk @@ Constraint.drop_nonlinear
            @@ Constraint.mk_and (TL.guard t)
                 (if PolyFunctionCall.has_function_calls update then
                    Constraint.mk_true
                  else
                    Constraint.mk_eq (Polynomial.of_var v') (PolyFunctionCall.to_poly update))
          in
          let update_vars =
            Set.union (PolyFunctionCall.vars update)
              (Set.inter (VarSet.singleton var) (Guard.vars @@ TL.guard t))
          in
          try
            (* thrown if solver does not know a solution due to e.g. non-linear arithmetic *)
            (* We have to intersect update_vars with the program vars in order to eliminate temporary variables from local size bounds*)
            find_bound (Set.inter program_vars update_vars) v' update_formula
              (s_range (PolyFunctionCall.to_poly update))
            |> Option.map ~f:(Tuple2.map1 from_t_to_trec)
          with
          | SMT.SMTFailure _ -> None
      else
        from_update_polyrec program_vars var
          (PolyFunctionCall.of_poly
          @@ Map.find_default
               (VarFunctionCall.update @@ RV.function_call_ m)
               ~default:(Polynomial.of_var var) var)
    in
    Logger.with_log logger Logger.DEBUG
      (fun () -> ("compute_bound", [ ("rv", RV.to_id_string (m, var)); ("var", Var.to_string var) ]))
      ~result:to_string_option_tuple execute


  let sizebound_local_with_equality program t v = compute_bound (P.input_vars program) t v
  let sizebound_local program t v = Option.map ~f:Tuple2.first @@ sizebound_local_with_equality program t v
end

include Make (TransitionLabel_) (Transition_) (RVGTypes.RV) (Program_)
