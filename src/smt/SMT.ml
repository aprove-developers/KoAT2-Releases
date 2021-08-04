open Batteries
open Formulas
open Polynomials
open BoundsInst

module VarMap = Map.Make(Var)

exception SMTFailure of string

let from_poly context =
  Polynomial.fold
    ~const:(fun value -> Z3.Arithmetic.Integer.mk_numeral_i context (OurInt.to_int value))
    ~var:(fun var -> if Var.is_real var then
                      (Z3.Arithmetic.Integer.mk_const_s context (Var.to_string var))
                     else
                       Z3.Arithmetic.Integer.mk_const_s context (Var.to_string var))
    ~neg:(Z3.Arithmetic.mk_unary_minus context)
    ~plus:(fun p1 p2 -> Z3.Arithmetic.mk_add context [p1; p2])
    ~times:(fun p1 p2 -> Z3.Arithmetic.mk_mul context [p1; p2])
    (* Somehow Z3.Arithmetic.mk_power makes Z3 use real arithmetic.. *)
    ~pow:(fun b e -> if e = 1 then b else if e > 1 then Z3.Arithmetic.mk_mul context (List.make e b) else failwith "Polynomial exponents should be between 1 and n")

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
  let from_finite_bound =
    RealBound.fold_bound
      ~const:(fun value -> Z3.Arithmetic.Real.mk_numeral_s context (OurFloat.to_string value))
      ~var:(fun var ->
        let varexp =
          if Var.is_real var then
            Z3.Arithmetic.Real.mk_const_s context (Var.to_string var)
          else
            Z3.Arithmetic.Integer.mk_const_s context (Var.to_string var)
        in
        Z3.Boolean.mk_ite context
          (Z3.Arithmetic.mk_gt context varexp (Z3.Arithmetic.Integer.mk_numeral_s context @@ OurInt.to_string OurInt.zero))
          varexp
          (Z3.Arithmetic.mk_unary_minus context varexp)
      )
      ~plus:(fun p1 p2 -> Z3.Arithmetic.mk_add context [p1; p2])
      ~times:(fun p1 p2 -> Z3.Arithmetic.mk_mul context [p1; p2])
      ~exp:(fun b e -> Z3.Arithmetic.mk_power context (Z3.Arithmetic.Real.mk_numeral_s context @@ OurFloat.to_string b) e)
  in
  match Option.map from_finite_bound (RealBound.prove_finiteness bound) with
  | Some b -> b
  | None -> raise (SMTFailure "inf not supported in SMT-Solving")

let from_bound context bound =
  let from_finite_bound =
    Bound.fold_bound
      ~const:(fun value -> Z3.Arithmetic.Integer.mk_numeral_s context (OurInt.to_string value))
      ~var:(fun var ->
        let varexp =
          if Var.is_real var then
            Z3.Arithmetic.Real.mk_const_s context (Var.to_string var)
          else
            Z3.Arithmetic.Integer.mk_const_s context (Var.to_string var)
        in
        Z3.Boolean.mk_ite context
          (Z3.Arithmetic.mk_gt context varexp (Z3.Arithmetic.Integer.mk_numeral_s context @@ OurInt.to_string OurInt.zero))
          varexp
          (Z3.Arithmetic.mk_unary_minus context varexp)
      )
      ~plus:(fun p1 p2 -> Z3.Arithmetic.mk_add context [p1; p2])
      ~times:(fun p1 p2 -> Z3.Arithmetic.mk_mul context [p1; p2])
      ~exp:(fun b e -> Z3.Arithmetic.mk_power context (Z3.Arithmetic.Integer.mk_numeral_s context @@ OurInt.to_string b) e)
  in
  match Option.map from_finite_bound (Bound.prove_finiteness bound) with
  | Some b -> b
  | None -> raise (SMTFailure "inf not supported in SMT-Solving")

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
    let init formula_from_context =
      let ctx = (Z3.mk_context [("model", "true"); ("proof", "false")]) in
      let z3formula = formula_from_context ctx in
      let solver = (Z3.Solver.mk_solver ctx None) in
      (Z3.Solver.add solver [z3formula]) ;
      solver

    let result_is solver expected_result =
      let result = Z3.Solver.check solver [] in
      if result == Z3.Solver.UNKNOWN then
        raise (SMTFailure ("SMT-Solver does not know a solution due to: " ^ Z3.Solver.get_reason_unknown solver))
      else
        result == expected_result

    let satisfiable formula =
      let solver = init (fun ctx -> from_real_formula ctx formula) in
      result_is solver Z3.Solver.SATISFIABLE

    let unsatisfiable formula =
      let solver = init (fun ctx -> from_real_formula ctx formula) in
      result_is solver Z3.Solver.UNSATISFIABLE

    let satisfiable_int formula =
      let solver = init (fun ctx -> from_formula ctx formula) in
      result_is solver Z3.Solver.SATISFIABLE

    let unsatisfiable_int formula =
      let solver = init (fun ctx -> from_formula ctx formula) in
      result_is solver Z3.Solver.UNSATISFIABLE


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
        | UNKNOWN -> raise (SMTFailure ("Z3 does can not find a result due to " ^ Z3.Solver.get_reason_unknown solver))

    let bound_lt_zero formula bound =
      let ctx = (Z3.mk_context [("model", "true"); ("proof", "false")]) in
      let bound_lt_zero = Z3.Arithmetic.mk_lt ctx (from_real_bound ctx bound) (from_real_poly ctx RealPolynomial.zero) in
      let solver = (Z3.Solver.mk_solver ctx None) in
      (Z3.Solver.add solver [from_real_formula ctx formula;bound_lt_zero]) ;
      let res = (Z3.Solver.check solver []) in
      match res with
        | SATISFIABLE -> true
        | UNSATISFIABLE -> false
        | UNKNOWN -> raise (SMTFailure ("Z3 does can not find a result due to " ^ Z3.Solver.get_reason_unknown solver))

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
        | UNKNOWN -> raise (SMTFailure ("Z3 does can not find a result due to " ^ Z3.Solver.get_reason_unknown solver))

    let to_string formula =
      Z3.Solver.to_string @@ init (fun ctx -> from_real_formula ctx formula)

  end

(** SMT solver which uses the microsoft project Z3 *)
module Z3Opt =
  struct
    module Valuation = Valuation.Make(OurInt)

    let context = ref (
                      Z3.mk_context [
                          ("model", "false");
                          ("proof", "false");
                        ]
                    )

    let result_is expected_result formula =
      let formula = from_formula !context formula in
      let optimisation_goal = Z3.Solver.mk_simple_solver !context in
      Z3.Solver.add optimisation_goal [formula];
      let result = Z3.Solver.check optimisation_goal [] in
      if result == Z3.Solver.UNKNOWN then
        raise (SMTFailure ("SMT-Solver does not know a solution due to: " ^ Z3.Solver.get_reason_unknown optimisation_goal))
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

(* Old Incrementel Solver for integer arithmetic. *)
module IncrementalZ3SolverInt =
  struct
    type t = Z3.Optimize.optimize * Z3.context

    module Valuation = Valuation.Make(OurInt)

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
           raise (SMTFailure ("SMT-Solver does not know a solution due to: " ^ Z3.Optimize.get_reason_unknown opt))
         else
           result == expected

    let satisfiable =
      result_is Z3.Solver.SATISFIABLE

    let unsatisfiable =
      result_is Z3.Solver.UNSATISFIABLE

    let to_string (opt,_) =
      Z3.Optimize.to_string opt

    let add (opt,context) formula =
      formula
      |> from_formula context
      |> fun formula -> Z3.Optimize.add opt [formula]

(*    let minimize opt vars =
      ignore (Z3.Optimize.minimize opt (minimisation_goal opt vars))
 *)
    (* Different try *)
    let minimize_absolute (opt,context) vars =
      vars
      |> List.iter (fun var ->
             ignore (Z3.Optimize.add_soft opt (from_formula context Formula.Infix.(Polynomial.of_var var <= Polynomial.zero)) (Var.to_string var) (Z3.Symbol.mk_int context 1));
             ignore (Z3.Optimize.add_soft opt (from_formula context Formula.Infix.(Polynomial.of_var var >= Polynomial.zero)) (Var.to_string var) (Z3.Symbol.mk_int context 1))
           )

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
                            |> Z3.Arithmetic.Integer.get_big_int
                            |> Z.to_string
                            |> OurInt.of_string
                          else
                            expr
                            |> Z3.Arithmetic.Real.get_ratio
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
    type t = Z3.context * Z3.Solver.solver * Z3.Optimize.optimize

    module RealValuation = Valuation.Make(OurFloat)
    module Valuation = Valuation.Make(OurInt)

    let to_string (ctx,slv,opt) = Z3.Optimize.to_string opt

    let create ?(model=true) ?(timeout=None) () =
      let context =
        Z3.mk_context @@
          (if Option.is_none timeout then [] else [("timeout", Int.to_string @@ Float.to_int @@ 1000. *. Option.get timeout)])
          @
          [
            ("model", if model then "true" else "false");
            ("proof", "false");
          ]
      in
      context, Z3.Solver.mk_solver context None, Z3.Optimize.mk_opt context

    let opt (_,_,opt) = opt
    let slv (_,slv,_) = slv

    let push t =
      Z3.Optimize.push (opt t);
      Z3.Solver.push (slv t)

    let pop t =
      Z3.Optimize.pop (opt t);
      Z3.Solver.pop (slv t) 1

    let result_is expected (_,slv,_) =
      Z3.Solver.check slv []
      |> fun result ->
         if result = Z3.Solver.UNKNOWN then
           raise (SMTFailure ("SMT-Solver does not know a solution due to: " ^ Z3.Solver.get_reason_unknown slv))
         else
           result = expected

    let satisfiable =
      result_is Z3.Solver.SATISFIABLE

    let satisfiable_option t =
      try
        Some (result_is Z3.Solver.SATISFIABLE t)
      with
        (SMTFailure _) -> None

    let unsatisfiable =
      result_is Z3.Solver.UNSATISFIABLE

    let unsatisfiable_option t =
      try
        Some (result_is Z3.Solver.UNSATISFIABLE t)
      with
        (SMTFailure _) -> None

    let add (ctx,slv,opt) formula =
      formula
      |> from_formula ctx
      |> fun formula -> (Z3.Solver.add slv [formula]; Z3.Optimize.add opt [formula])

    let add_z3 (ctx,slv,opt) formula =
      Z3.Solver.add slv [formula]; Z3.Optimize.add opt [formula]

    let add_real (ctx,slv,opt) formula =
      formula
      |> from_real_formula ctx
      |> add_z3 (ctx,slv,opt)


    (* Get the value of a constant Z3 expression *)
    let get_expr_value ~val_of_intstr ~val_of_ratio expr =
      if Z3.Arithmetic.is_int expr then
        expr
        |> Z3.Arithmetic.Integer.numeral_to_string
        |> val_of_intstr
      else
        expr
        |> Z3.Arithmetic.Real.get_ratio
        (* TODO Round shouldnt be the solution, but do we need this anyway, since we ignore the values of helper variables? *)
        |> val_of_ratio


    let get_int_expr_value =
       get_expr_value ~val_of_intstr:OurInt.of_string ~val_of_ratio:(OurInt.of_string % Z.to_string % Q.to_bigint)

    let get_real_expr_value =
      get_expr_value ~val_of_intstr:(OurFloat.of_ourint % OurInt.of_string) ~val_of_ratio:(OurFloat.of_string % Q.to_string)

    (* Optimisation functions *)
    (* Different try *)
    let minimize_absolute_old (ctx,slv,opt) vars =
      vars
      |> List.iter (fun var ->
             ignore (Z3.Optimize.add_soft opt (from_formula ctx Formula.Infix.(Polynomial.of_var var <= Polynomial.zero)) (Var.to_string var) (Z3.Symbol.mk_int ctx 1));
             ignore (Z3.Optimize.add_soft opt (from_formula ctx Formula.Infix.(Polynomial.of_var var >= Polynomial.zero)) (Var.to_string var) (Z3.Symbol.mk_int ctx 1))
           )

    let absolute_value context (var: Var.t) =
      Z3.Boolean.mk_ite context
        (from_real_formula context RealFormula.Infix.((RealPolynomial.of_var var) <= RealPolynomial.zero))
        (from_real_poly context (RealPolynomial.sub RealPolynomial.zero (RealPolynomial.of_var var)) )
        (from_real_poly context (RealPolynomial.of_var var))

    let minimize_absolute_with_weight (ctx,slv,opt) vars_with_weight =
      vars_with_weight
      |> List.map (fun (v,w) -> Z3.Arithmetic.mk_mul ctx [Z3.Arithmetic.Real.mk_numeral_s ctx (OurFloat.to_string w); absolute_value ctx v])
      |> Z3.Arithmetic.mk_add ctx
      |> Z3.Optimize.minimize opt
      |> ignore

    let minimize_absolute t vars =
      minimize_absolute_with_weight t (List.map (fun v -> v, OurFloat.one) vars)

    let minimize_absolute_iteratively (ctx,slv,opt) vars =
      vars
      |> List.map (absolute_value ctx)
      |> List.iter (fun expr -> ignore @@ Z3.Optimize.minimize opt expr)
      |> ignore

    let minimize (ctx,slv,opt) var =
      ignore (Z3.Optimize.minimize opt (from_poly ctx (Polynomial.of_var var)))

    let maximize (ctx,slv,opt) var =
      ignore (Z3.Optimize.maximize opt (from_poly ctx (Polynomial.of_var var)))

    let extract_values optimized (ctx,slv,opt) ~get_z3_value =
      let extract_from_model model =
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
                |> get_z3_value
              in
              (var, value))
      in
      let model =
        if not optimized then
          if Z3.Solver.check slv [] == Z3.Solver.SATISFIABLE then
            Z3.Solver.get_model slv
          else
            None
        else
          let opt_res = Z3.Optimize.check opt in
          match opt_res with
          | Z3.Solver.SATISFIABLE ->
              Z3.Optimize.get_model opt
          | Z3.Solver.UNSATISFIABLE ->
              None
          | Z3.Solver.UNKNOWN ->
              if Z3.Solver.check slv [] == Z3.Solver.SATISFIABLE then
                Z3.Solver.get_model slv
              else
                None
      in
      model
      |> Option.map extract_from_model

    let model ?(optimized=true) (ctx,slv,opt) =
      extract_values optimized (ctx,slv,opt) ~get_z3_value:get_int_expr_value
      |> Option.map Valuation.from

    let model_real ?(optimized=true) (ctx,slv,opt) =
      extract_values optimized (ctx,slv,opt) ~get_z3_value:get_real_expr_value
      |> Option.map RealValuation.from

    let minimise_absolute_with_int_binary_search (ctx,slv,opt) var =
      let add_constraint middle  = add_z3 (ctx,slv,opt) @@ Z3.Arithmetic.mk_le ctx (absolute_value ctx var) (from_poly ctx @@ Polynomial.of_constant middle) in
      let rec binsearch low high =
        if OurInt.compare low high >= 0 then
          ()
        else
          let middle = OurInt.div (OurInt.add low high) (OurInt.of_int 2) in
          push (ctx,slv,opt);
          add_constraint middle;
          let satisfiable = satisfiable (ctx,slv,opt) in
          if satisfiable then
            binsearch low middle
          else
            (pop (ctx,slv,opt); binsearch (OurInt.add middle OurInt.one) high)
      in

      let curr_model = model (ctx,slv,opt) in
      if List.exists (Var.equal var) (Valuation.vars @@ Option.get curr_model) then
        let initial = OurInt.abs (Valuation.eval var @@ Option.get @@ model (ctx,slv,opt)) in
        add_constraint initial;
        binsearch OurInt.zero initial

    let minimise_absolute_ints_binary_search_iteratively t vars =
      List.iter (minimise_absolute_with_int_binary_search t) vars

    let is_zero ctx (var: Var.t) =
      Z3.Boolean.mk_ite ctx
        (from_real_formula ctx RealFormula.Infix.(RealPolynomial.of_var var = RealPolynomial.zero))
        (from_real_poly ctx RealPolynomial.zero)
        (from_real_poly ctx RealPolynomial.one)

    let minimize_set_vars (ctx,slv,opt) ?(add_as_constraint=false) vars =
      let varformulas = VarMap.of_enum @@ Enum.map (fun v -> v,is_zero ctx v) @@ List.enum vars in
      let formula = Z3.Arithmetic.mk_add ctx @@ List.map Tuple2.second @@ VarMap.bindings varformulas in
      if add_as_constraint then
        (* We will not need the optimizers resulting state since we are adding a new constraint anyway *)
        Z3.Optimize.push opt;

      let handle = Z3.Optimize.minimize opt formula in
      if add_as_constraint then
        (
          ignore (Z3.Optimize.check opt);
          let model = model_real (ctx, slv, opt) |> Option.get in

          Z3.Optimize.pop opt;

          let enforce_formulas =
            VarMap.bindings varformulas
            |> List.map (
                  fun (v, f) ->
                    if OurFloat.equal (RealValuation.eval v model) OurFloat.zero then
                      [ from_real_formula ctx RealFormula.Infix.(RealPolynomial.of_var v =  RealPolynomial.zero)]
                    else
                      []
                )
            |> List.flatten
            |> Z3.Boolean.mk_and ctx
          in

          (* Add enforcing formula and push the new state *)
          push (ctx,slv,opt);
          add_z3 (ctx,slv,opt) enforce_formulas
        )
      else
        ignore handle

  end

  (* Old Incrementel Solver for integer arithmetic. *)
module SolverFast =
  struct
    type t = Z3.Solver.solver * Z3.context

    module Valuation = Valuation.Make(OurInt)

    let create ?(model=true) () =
      let context =
        Z3.mk_context [
            ("model", if model then "true" else "false");
            ("proof", "false");
            (* ("timeout", "10"); *)
          ]
      in
      Z3.Solver.mk_simple_solver context, context

    let solver (solver,_) = solver

    let push =
      Z3.Solver.push % solver

    let pop t =
      Z3.Solver.pop (solver t) 1

    let result_is expected (solver,_) =
      Z3.Solver.check solver []
      |> fun result ->
         if result == Z3.Solver.UNKNOWN then
           raise (SMTFailure ("SMT-Solver does not know a solution due to: " ^ Z3.Solver.get_reason_unknown solver))
         else
           result == expected

    let satisfiable =
      result_is Z3.Solver.SATISFIABLE

    let unsatisfiable =
      result_is Z3.Solver.UNSATISFIABLE

    let add (solver,context) formula =
      formula
      |> from_formula context
      |> fun formula -> Z3.Solver.add solver [formula]

    let add_bound_comparison (solver, ctx) cmpoperator b1 b2 =
      let z3_compoperator = match cmpoperator with
        | `LE -> Z3.Arithmetic.mk_le
        | `LT -> Z3.Arithmetic.mk_lt
      in
      let z3_b1 = from_bound ctx b1 in
      let z3_b2 = from_bound ctx b2 in
      Z3.Solver.add solver [ (z3_compoperator ctx) z3_b1 z3_b2]


    let model (solver,_) =
      if Z3.Solver.check solver [] == Z3.Solver.SATISFIABLE then
        solver
        |> Z3.Solver.get_model
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
                            |> Z3.Arithmetic.Integer.get_big_int
                            |> Z.to_string
                            |> OurInt.of_string
                          else
                            expr
                            |> Z3.Arithmetic.Real.get_ratio
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
