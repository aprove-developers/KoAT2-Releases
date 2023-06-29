open Batteries
open Formulas
open Polynomials
open BoundsInst

exception SMTFailure of string

let from_poly context =
  Polynomial.fold
    ~const:(fun value -> Z3.Arithmetic.Integer.mk_numeral_i context (OurInt.to_int value))
    ~indeterminate:(fun var -> if Var.is_real var then
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
    ~indeterminate:(fun var -> if Var.is_real var then
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

(** SMT solver which uses the microsoft project Z3 *)
module Z3Solver =
  struct
    module Valuation = Valuation.Make(OurInt)

    let mk_context = fun () -> Z3.mk_context [("model", "false"); ("proof", "false");]

    let version = Z3.Version.full_version

    let result_is expected_result formula =
      let context = mk_context () in
      let formula = from_formula context formula in
      let optimisation_goal = Z3.Solver.mk_simple_solver context in
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
      tautology Formula.Infix.(formula => (poly > Polynomial.zero))

    (** Returns true iff the formula implies the negativity of the polynomial*)
    let check_negativity (formula : Formula.t) (poly: Polynomial.t) =
      tautology Formula.Infix.(formula => (poly < Polynomial.zero))

    let get_model ?(coeffs_to_minimise=[]) formula =
      let context = mk_context () in
      let z3_expr = from_formula context formula in
      let optimisation_goal = Z3.Optimize.mk_opt context in
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
                            Z3.Arithmetic.Integer.get_big_int expr
                          else
                            Z3.Arithmetic.Real.get_ratio expr
                            (* TODO Round shouldnt be the solution, but do we need this anyway, since we ignore the values of helper variables? *)
                            |> Q.to_bigint
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


module Z3SolverTimeout =
  struct
    module Valuation = Valuation.Make(OurInt)

    let context = ref (
                      Z3.mk_context [
                          ("model", "false");
                          ("proof", "false");
                          ("timeout", "2000"); (* timeout (unsigned) default timeout (in milliseconds) used for solvers *)
                        ]
                    )

    let version = Z3.Version.full_version

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
                            Z3.Arithmetic.Integer.get_big_int expr
                          else
                            expr
                            |> Z3.Arithmetic.Real.get_ratio
                            (* TODO Round shouldnt be the solution, but do we need this anyway, since we ignore the values of helper variables? *)
                            |> Q.to_bigint
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
module IncrementalZ3Solver =
  struct
    type t = Z3.Solver.solver * Z3.context

    module IntValuation = Valuation.Make(OurInt)
    module RealValuation = Valuation.Make(OurFloat)

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
      Z3.Solver.add solver [from_formula context formula]

    let add_real (solver,context) formula =
      Z3.Solver.add solver [from_real_formula context formula]

    let add_bound_comparison (solver, ctx) cmpoperator b1 b2 =
      let z3_compoperator = match cmpoperator with
        | `LE -> Z3.Arithmetic.mk_le
        | `LT -> Z3.Arithmetic.mk_lt
      in
      let z3_b1 = from_bound ctx b1 in
      let z3_b2 = from_bound ctx b2 in
      Z3.Solver.add solver [ (z3_compoperator ctx) z3_b1 z3_b2]

    let add_realbound_comparison (solver, ctx) cmpoperator b1 b2 =
      let z3_compoperator = match cmpoperator with
        | `LE -> Z3.Arithmetic.mk_le
        | `LT -> Z3.Arithmetic.mk_lt
      in
      let z3_b1 = from_real_bound ctx b1 in
      let z3_b2 = from_real_bound ctx b2 in
      Z3.Solver.add solver [ (z3_compoperator ctx) z3_b1 z3_b2]


    let extract_values ((slv,ctx): t) ~get_z3_value =
      if result_is Z3.Solver.SATISFIABLE (slv,ctx) then
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
        Z3.Solver.get_model slv
        |> Option.map extract_from_model
      else
        None

    (* Get the value of a constant Z3 expression *)
    let get_expr_value ~val_of_z3int ~val_of_ratio expr =
      if Z3.Arithmetic.is_int expr then
        expr
        |> Z3.Arithmetic.Integer.get_big_int
        |> val_of_z3int
      else
        expr
        |> Z3.Arithmetic.Real.get_ratio
        (* TODO Round shouldnt be the solution, but do we need this anyway, since we ignore the values of helper variables? *)
        |> val_of_ratio

    let get_int_expr_value =
       get_expr_value ~val_of_z3int:identity ~val_of_ratio:Q.to_bigint

    let get_real_expr_value =
      get_expr_value ~val_of_z3int:OurFloat.of_ourint ~val_of_ratio:identity

    let model t =
      extract_values t ~get_z3_value:get_int_expr_value
      |> Option.map IntValuation.from

    let model_real t =
      extract_values t  ~get_z3_value:get_real_expr_value
      |> Option.map RealValuation.from

  end
