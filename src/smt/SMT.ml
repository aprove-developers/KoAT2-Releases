open Batteries
open Formulas
open Polynomials
   
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
  
let from_formula context =
  Formula.fold 
    ~subject:(from_poly context)
    ~le:(Z3.Arithmetic.mk_le context)
    ~correct:(Z3.Boolean.mk_true context)
    ~conj:(fun a1 a2 -> Z3.Boolean.mk_and context [a1; a2])
    ~wrong:(Z3.Boolean.mk_false context)
    ~disj:(fun a1 a2 -> Z3.Boolean.mk_or context [a1; a2])

(** SMT solver which uses the microsoft project Z3 *)
module Z3Solver =
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
