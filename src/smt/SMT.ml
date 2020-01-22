open Batteries
open Formulas
open Polynomials
open BoundsInst

let from_poly context =
  Polynomial.fold
    ~const:(fun value -> Z3.Arithmetic.Integer.mk_numeral_i context (OurInt.to_int value))
    ~var:(fun var -> if Var.is_real var then
                       Z3.Arithmetic.Real.mk_const_s context (Var.to_string var)
                     else
                       Z3.Arithmetic.Integer.mk_const_s context (Var.to_string var))
    ~neg:(Z3.Arithmetic.mk_unary_minus context)
    ~plus:(fun p1 p2 -> Z3.Arithmetic.mk_add context [p1; p2])
    ~times:(fun p1 p2 -> Z3.Arithmetic.mk_mul context [p1; p2])
    ~pow:(fun b e -> Z3.Arithmetic.mk_power context b (Z3.Arithmetic.Integer.mk_numeral_i context e))

let from_real_poly context =
  RealPolynomial.fold
    ~const:(fun value -> Z3.Arithmetic.Real.mk_numeral_s context (OurFloat.to_string value))
    ~var:(fun var -> if Var.is_real var then
                       Z3.Arithmetic.Real.mk_const_s context (Var.to_string var)
                     else
                       Z3.Arithmetic.Integer.mk_const_s context (Var.to_string var))
    ~neg:(Z3.Arithmetic.mk_unary_minus context)
    ~plus:(fun p1 p2 -> Z3.Arithmetic.mk_add context [p1; p2])
    ~times:(fun p1 p2 -> Z3.Arithmetic.mk_mul context [p1; p2])
    ~pow:(fun b e -> Z3.Arithmetic.mk_power context b (Z3.Arithmetic.Real.mk_numeral_i context e))

let from_real_bound context bound =
  let liftA2 f x x' =
    let f1 = Option.map f x in
    match f1 with
    | (Some f2) -> Option.map f2 x'
    | None -> None
  in

  let boundm =
    RealBound.fold
      ~const:(fun value -> Some (Z3.Arithmetic.Real.mk_numeral_s context (OurFloat.to_string value)))
      ~var:(fun var -> if Var.is_real var then
                         Some (Z3.Arithmetic.Real.mk_const_s context (Var.to_string var))
                       else
                         Some (Z3.Arithmetic.Integer.mk_const_s context (Var.to_string var)))
      ~neg:(Option.map (Z3.Arithmetic.mk_unary_minus context))
      ~plus:(liftA2 (fun p1 p2 -> Z3.Arithmetic.mk_add context [p1; p2]))
      ~times:(liftA2 (fun p1 p2 -> Z3.Arithmetic.mk_mul context [p1; p2]))
      ~exp:(fun b -> Option.map (fun e -> Z3.Arithmetic.mk_power context (Z3.Arithmetic.Real.mk_numeral_s context (OurFloat.to_string b)) e))
      ~max:(liftA2 (fun a b -> Z3.Boolean.mk_ite context (Z3.Arithmetic.mk_gt context a b) a b))
      ~min:(liftA2 (fun a b -> Z3.Boolean.mk_ite context (Z3.Arithmetic.mk_lt context a b) a b))
      ~abs:(Option.map (fun a -> Z3.Boolean.mk_ite context (Z3.Arithmetic.mk_gt context a
                                                             (Z3.Arithmetic.Real.mk_numeral_s context
                                                                @@ OurFloat.to_string OurFloat.zero))
                         a (Z3.Arithmetic.Real.mk_numeral_s context @@ OurFloat.to_string OurFloat.zero)))
      ~inf:(None)
      bound
  in
  match boundm with
  | Some b -> b
  | None -> raise (Failure "inf not supported in SMT-Solving")

let from_formula context =
  Formula.fold
    ~subject:(from_poly context)
    ~le:(Z3.Arithmetic.mk_le context)
    ~lt:(Z3.Arithmetic.mk_lt context)
    ~correct:(Z3.Boolean.mk_true context)
    ~conj:(fun a1 a2 -> Z3.Boolean.mk_and context [a1; a2])
    ~wrong:(Z3.Boolean.mk_false context)
    ~disj:(fun a1 a2 -> Z3.Boolean.mk_or context [a1; a2])

let from_real_formula context =
  RealFormula.fold
    ~subject:(from_real_poly context)
    ~le:(Z3.Arithmetic.mk_le context)
    ~lt:(Z3.Arithmetic.mk_lt context)
    ~correct:(Z3.Boolean.mk_true context)
    ~conj:(fun a1 a2 -> Z3.Boolean.mk_and context [a1; a2])
    ~wrong:(Z3.Boolean.mk_false context)
    ~disj:(fun a1 a2 -> Z3.Boolean.mk_or context [a1; a2])

module Z3Solver =
  struct
    let init formula =
      let ctx = (Z3.mk_context [("model", "true"); ("proof", "false")]) in
      let z3formula = from_real_formula ctx formula in
      let solver = (Z3.Solver.mk_solver ctx None) in
      (Z3.Solver.add solver [z3formula]) ;
      solver

    let satisfiable formula =
      let solver = init formula in
      let res = (Z3.Solver.check solver []) in
      match res with
        | SATISFIABLE -> true
        | UNSATISFIABLE -> false
        | UNKNOWN -> raise (Failure ("Z3 does can not find a result due to " ^ Z3.Solver.get_reason_unknown solver))

    (* TODO sinnvoll in formulas integrieren *)
    let bound_gt_zero formula bound =
      let ctx = (Z3.mk_context [("model", "true"); ("proof", "false")]) in
      let bound_gt_zero = Z3.Arithmetic.mk_gt ctx (from_real_bound ctx bound) (from_real_poly ctx RealPolynomial.zero) in
      let solver = (Z3.Solver.mk_solver ctx None) in
      (Z3.Solver.add solver [from_real_formula ctx formula;bound_gt_zero]) ;
      let res = (Z3.Solver.check solver []) in
      match res with
        | SATISFIABLE -> true
        | UNSATISFIABLE -> false
        | UNKNOWN -> raise (Failure ("Z3 does can not find a result due to " ^ Z3.Solver.get_reason_unknown solver))

    let bound_lt_zero formula bound =
      let ctx = (Z3.mk_context [("model", "true"); ("proof", "false")]) in
      let bound_lt_zero = Z3.Arithmetic.mk_lt ctx (from_real_bound ctx bound) (from_real_poly ctx RealPolynomial.zero) in
      let solver = (Z3.Solver.mk_solver ctx None) in
      (Z3.Solver.add solver [from_real_formula ctx formula;bound_lt_zero]) ;
      let res = (Z3.Solver.check solver []) in
      match res with
        | SATISFIABLE -> true
        | UNSATISFIABLE -> false
        | UNKNOWN -> raise (Failure ("Z3 does can not find a result due to " ^ Z3.Solver.get_reason_unknown solver))

    let cmp_bounds inv comperator b1 b2 =
      let ctx = (Z3.mk_context [("model", "true"); ("proof", "false")]) in
      let expr = match comperator with
        | `GT -> Z3.Arithmetic.mk_le ctx (from_real_bound ctx b1) (from_real_bound ctx b2)
        | `GE -> Z3.Arithmetic.mk_lt ctx (from_real_bound ctx b1) (from_real_bound ctx b2)
      in
      let solver = (Z3.Solver.mk_solver ctx None) in
      (Z3.Solver.add solver [from_real_formula ctx inv; expr]);
      let res = Z3.Solver.check solver [] in
      match res with
        | SATISFIABLE -> false
        | UNSATISFIABLE -> true
        | UNKNOWN -> raise (Failure ("Z3 does can not find a result due to " ^ Z3.Solver.get_reason_unknown solver))

    let to_string formula =
      Z3.Solver.to_string (init formula)

  end

(** SMT solver which uses the microsoft project Z3 *)
module Z3Opt =
  struct
    module Valuation = Valuation.Make(OurInt)

    let context = ref (
                      Z3.mk_context [
                          ("model", "true");
                          ("proof", "false");
                        ]
                    )

    let result_is expected_result formula =
      let formula = from_formula !context formula in
      let optimisation_goal = Z3.Optimize.mk_opt !context in
      Z3.Optimize.add optimisation_goal [formula];
      let result = Z3.Optimize.check optimisation_goal in
      if result == Z3.Solver.UNKNOWN then
        raise (Failure ("SMT-Solver does not know a solution due to: " ^ Z3.Optimize.get_reason_unknown optimisation_goal))
      else
        result == expected_result

    (** checks if there exists a satisfying assignment for a given formula
        uses Z3 optimisation methods*)
    let satisfiable formula =
      result_is Z3.Solver.SATISFIABLE formula

    let unsatisfiable formula =
      result_is Z3.Solver.UNSATISFIABLE formula

    let tautology =
      unsatisfiable % Formula.neg

    let equivalent formula1 formula2 =
      (* Negating formula1 <=> formula2 *)
      Formula.Infix.((formula1 && Formula.neg formula2) || (formula2 && Formula.neg formula1))
      |> unsatisfiable

    (** Returns true iff the formula implies the positivity of the polynomial*)
    let check_positivity (formula : Formula.t) (poly: Polynomial.t) =
      tautology Formula.Infix.(formula => (poly >= Polynomial.zero))

    (** Returns true iff the formula implies the negativity of the polynomial*)
    let check_negativity (formula : Formula.t) (poly: Polynomial.t) =
      tautology Formula.Infix.(formula => (poly <= Polynomial.zero))

    let minimisation_goal (formula : Formula.t) (coeffs_to_minimise: Var.t list): Z3.Expr.expr =
      let generator poly v =
        if check_positivity formula Polynomial.(of_var v) then
          Polynomial.(poly + of_var v)
        else if check_negativity formula Polynomial.(of_var v) then
          Polynomial.(poly - of_var v)
        else
          poly
      in
      from_poly !context (List.fold_left generator Polynomial.zero coeffs_to_minimise)

    let get_model ?(coeffs_to_minimise=[]) formula =
      let z3_expr = from_formula !context formula in
      let optimisation_goal = Z3.Optimize.mk_opt !context in
      Z3.Optimize.add optimisation_goal [z3_expr];
      let status = Z3.Optimize.check optimisation_goal in
      if status == Z3.Solver.SATISFIABLE then
        optimisation_goal
        |> Z3.Optimize.get_model
        |> Option.map (fun model ->
               model
               |> Z3.Model.get_const_decls
               |> List.map (fun func_decl ->
                      let var =
                        func_decl
                        |> Z3.FuncDecl.get_name
                        |> Z3.Symbol.get_string
                        |> Var.of_string
                      in
                      let value =
                        func_decl
                        |> Z3.Model.get_const_interp model
                        |> Option.get (* Should be fine here *)
                        |> (fun expr ->
                          if Z3.Arithmetic.is_int expr then
                            expr
                            |> Z3.Arithmetic.Integer.numeral_to_string
                            |> OurInt.of_string
                          else
                            expr
                            |> Z3.Arithmetic.Real.get_ratio
                            (* TODO Round shouldnt be the solution, but do we need this anyway, since we ignore the values of helper variables? *)
                            |> Q.to_bigint
                            |> Z.to_string
                            |> OurInt.of_string
                        )
                      in
                      (var, value)
                    )
             )
        |? []
        |> Valuation.from
        |> Option.some
      else None

  end

module IncrementalZ3Solver =
  struct
    type t = Z3.Optimize.optimize * Z3.context

    module RealValuation = Valuation.Make(OurFloat)
    module Valuation = Valuation.Make(OurInt)

    let to_string (opt,_) = Z3.Optimize.to_string opt

    let create ?(model=true) () =
      let context =
        Z3.mk_context [
            ("model", if model then "true" else "false");
            ("proof", "false");
          ]
      in
      Z3.Optimize.mk_opt context, context

    let opt (opt,_) = opt

    let push =
      Z3.Optimize.push % opt

    let pop =
      Z3.Optimize.pop % opt

    let result_is expected (opt,_) =
      opt
      |> Z3.Optimize.check
      |> fun result ->
         if result == Z3.Solver.UNKNOWN then
           raise (Failure ("SMT-Solver does not know a solution due to: " ^ Z3.Optimize.get_reason_unknown opt))
         else
           result == expected

    let satisfiable =
      result_is Z3.Solver.SATISFIABLE

    let unsatisfiable =
      result_is Z3.Solver.UNSATISFIABLE

    let add (opt,context) formula =
      formula
      |> from_formula context
      |> fun formula -> Z3.Optimize.add opt [formula]

    let add_real (opt,context) formula =
      formula
      |> from_real_formula context
      |> fun formula -> Z3.Optimize.add opt [formula]

    (** Returns true iff the formula implies the positivity of the variable. *)
    let is_positive (opt,_) (var: Var.t) =
      push opt;
      add opt Formula.Infix.(Polynomial.of_var var >= Polynomial.zero);
      let result = unsatisfiable opt in
      pop opt;
      result

    (** Returns true iff the formula implies the negativity of the variable. *)
    let is_negative (opt,_) (var: Var.t) =
      push opt;
      add opt Formula.Infix.(Polynomial.of_var var <= Polynomial.zero);
      let result = unsatisfiable opt in
      pop opt;
      result

    let minimisation_goal (opt,context) (vars: Var.t list): Z3.Expr.expr =
      let generator poly v =
        if is_positive opt v then
          Polynomial.(poly + of_var v)
        else if is_negative opt v then
          Polynomial.(poly - of_var v)
        else
          poly
      in
      from_poly context (List.fold_left generator Polynomial.zero vars)

(*    let minimize opt vars =
      ignore (Z3.Optimize.minimize opt (minimisation_goal opt vars))
 *)
    (* Different try *)
    let minimize_absolute_with_weight (opt,context) vars_with_weight =
      vars_with_weight
      |> List.iter (fun (var,weight) ->
             ignore (Z3.Optimize.add_soft opt (from_formula context Formula.Infix.(Polynomial.of_var var <= Polynomial.zero)) (Var.to_string var) (Z3.Symbol.mk_int context weight));
             ignore (Z3.Optimize.add_soft opt (from_formula context Formula.Infix.(Polynomial.of_var var >= Polynomial.zero)) (Var.to_string var) (Z3.Symbol.mk_int context weight))
           )
    let minimize_absolute (opt,context) vars =
      minimize_absolute_with_weight (opt,context) (List.map (fun v -> v,1) vars)

    let minimize_absolute_v2 (opt,context) vars =
      let absolute_value (var: Var.t) =
        Z3.Boolean.mk_ite context
          (from_real_formula context RealFormula.Infix.((RealPolynomial.of_var var) <= RealPolynomial.zero))
          (from_real_poly context (RealPolynomial.of_var var))
          (from_real_poly context (RealPolynomial.sub RealPolynomial.zero (RealPolynomial.of_var var)) )
      in
      vars
      |> List.map (absolute_value)
      |> Z3.Arithmetic.mk_add context
      |> Z3.Optimize.minimize opt
      |> ignore

    let minimize (opt,context) var =
      ignore (Z3.Optimize.minimize opt (from_poly context (Polynomial.of_var var)))

    let maximize (opt,context) var =
      ignore (Z3.Optimize.maximize opt (from_poly context (Polynomial.of_var var)))

    let model (opt,_) =
      if Z3.Optimize.check opt == Z3.Solver.SATISFIABLE then
        opt
        |> Z3.Optimize.get_model
        |> Option.map (fun model ->
               model
               |> Z3.Model.get_const_decls
               |> List.map (fun func_decl ->
                      let var =
                        func_decl
                        |> Z3.FuncDecl.get_name
                        |> Z3.Symbol.get_string
                        |> Var.of_string
                      in
                      let value =
                        func_decl
                        |> Z3.Model.get_const_interp model
                        |> Option.get (* Should be fine here *)
                        |> (fun expr ->
                          if Z3.Arithmetic.is_int expr then
                            expr
                            |> Z3.Arithmetic.Integer.numeral_to_string
                            |> OurInt.of_string
                          else
                            expr
                            |> Z3.Arithmetic.Real.get_ratio
                            (* TODO Round shouldnt be the solution, but do we need this anyway, since we ignore the values of helper variables? *)
                            |> Q.to_bigint
                            |> Z.to_string
                            |> OurInt.of_string
                        )
                      in
                      (var, value)
                    )
             )
        |? []
        |> Valuation.from
        |> Option.some
      else None

    let model_real (opt,_) =
      if Z3.Optimize.check opt == Z3.Solver.SATISFIABLE then
        opt
        |> Z3.Optimize.get_model
        |> Option.map (fun model ->
               model
               |> Z3.Model.get_const_decls
               |> List.map (fun func_decl ->
                      let var =
                        func_decl
                        |> Z3.FuncDecl.get_name
                        |> Z3.Symbol.get_string
                        |> Var.of_string
                      in
                      let value =
                        func_decl
                        |> Z3.Model.get_const_interp model
                        |> Option.get (* Should be fine here *)
                        |> (fun expr ->
                          if Z3.Arithmetic.is_int expr then
                            expr
                            |> Z3.Arithmetic.Integer.numeral_to_string
                            |> OurInt.of_string
                            |> OurFloat.of_ourint
                          else
                            expr
                            |> Z3.Arithmetic.Real.get_ratio
                            |> Q.to_string
                            |> OurFloat.of_string
                        )
                      in
                      (var, value)
                    )
             )
        |? []
        |> RealValuation.from
        |> Option.some
      else None

  end
