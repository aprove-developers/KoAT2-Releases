open Batteries
open Formulas
open Polynomials
   
(** SMT solver which uses the microsoft project Z3 *)
module Z3Solver =
  struct
    module Valuation = Valuation.Make(OurInt)
    type valuation = Valuation.t
    type formula = Formula.t
       
    let context = ref (
                      Z3.mk_context [
                          ("model", "true");
                          ("proof", "false");
                        ]
                    )

    let from_constraint (constraints : formula) =
      Formula.fold 
        ~subject:(
          Polynomial.fold
            ~const:(fun value -> Z3.Arithmetic.Integer.mk_numeral_i !context (OurInt.to_int value))
            ~var:(fun var -> if Var.is_real var then
                               Z3.Arithmetic.Real.mk_const_s !context (Var.to_string var)
                             else
                               Z3.Arithmetic.Integer.mk_const_s !context (Var.to_string var))
            ~neg:(Z3.Arithmetic.mk_unary_minus !context)
            ~plus:(fun p1 p2 -> Z3.Arithmetic.mk_add !context [p1; p2])
            ~times:(fun p1 p2 -> Z3.Arithmetic.mk_mul !context [p1; p2])
            ~pow:(fun b e -> Z3.Arithmetic.mk_power !context b (Z3.Arithmetic.Integer.mk_numeral_i !context e))
        )
        ~le:(Z3.Arithmetic.mk_le !context)
        ~correct:(Z3.Boolean.mk_true !context)
        ~conj:(fun a1 a2 -> Z3.Boolean.mk_and !context [a1; a2])
        ~wrong:(Z3.Boolean.mk_false !context)
        ~disj:(fun a1 a2 -> Z3.Boolean.mk_or !context [a1; a2])
        constraints
        
        
    let from_poly = 
      Polynomial.fold
            ~const:(fun value -> Z3.Arithmetic.Integer.mk_numeral_i !context (OurInt.to_int value))
            ~var:(fun var -> if Var.is_real var then
                               Z3.Arithmetic.Real.mk_const_s !context (Var.to_string var)
                             else
                               Z3.Arithmetic.Integer.mk_const_s !context (Var.to_string var))
            ~neg:(Z3.Arithmetic.mk_unary_minus !context)
            ~plus:(fun p1 p2 -> Z3.Arithmetic.mk_add !context [p1; p2])
            ~times:(fun p1 p2 -> Z3.Arithmetic.mk_mul !context [p1; p2])
            ~pow:(fun b e -> Z3.Arithmetic.mk_power !context b (Z3.Arithmetic.Integer.mk_numeral_i !context e))
(*    let to_string (constraints : Constraint.t) =
        let solver = Z3.Solver.mk_simple_solver !context in
        let formula = from_constraint constraints in
        let optimisation_goal = Z3.Optimize.mk_opt !context in
        Z3.Optimize.add optimisation_goal [formula];
        Z3.Optimize.to_string optimisation_goal*)

    let result_is expected_result formula =
        let formula = from_constraint formula in
        let optimisation_goal = Z3.Optimize.mk_opt !context in
        Z3.Optimize.add  optimisation_goal [formula];
        let result = Z3.Optimize.check optimisation_goal in
        if result == Z3.Solver.UNKNOWN then
          raise (Failure "SMT-Solver does not know a solution")
        else
          result == expected_result

    (** checks if there exists a satisfying assignment for a given constraint
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
    let check_positivity (formula : formula) (poly: Polynomial.t) =
      tautology Formula.Infix.(formula => (poly >= Polynomial.zero))
    
    (** Returns true iff the formula implies the negativity of the polynomial*)
    let check_negativity (formula : formula) (poly: Polynomial.t) =
      tautology Formula.Infix.(formula => (poly <= Polynomial.zero))
    
    let generate_minimise (constraints : formula) (coeffs_to_minimise: Var.t list) =
      let generator poly vrbl =
        if check_positivity constraints Polynomial.(of_var vrbl) then
          Polynomial.(poly + (of_var vrbl))
        else if check_negativity constraints Polynomial.(of_var vrbl) then
          Polynomial.(poly - (of_var vrbl))
        else
          poly
      in
      from_poly (List.fold_left generator Polynomial.zero coeffs_to_minimise)
    
    let get_model ?(coeffs_to_minimise=[]) formula =
      let z3_expr = from_constraint formula in
      let optimisation_goal = Z3.Optimize.mk_opt !context in
      Z3.Optimize.add optimisation_goal [z3_expr];
      (*let minimise_constr = List.map (fun var -> from_poly (Polynomial.pow (Polynomial.of_var var) 2)) coeffs_to_minimise in
        for i = 0 to (List.length minimise_constr) - 1 do
        Z3.Optimize.minimize optimisation_goal (List.at minimise_constr i);
        done;*)
      Z3.Optimize.minimize optimisation_goal (generate_minimise formula coeffs_to_minimise);
      (*print_string ("\nOptimise to_string:\n"^(Z3.Optimize.to_string optimisation_goal)^"\n");*)
      let status = Z3.Optimize.check optimisation_goal in
      if status == Z3.Solver.SATISFIABLE (*|| status == Z3.Solver.UNKNOWN*) then
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
                            Z3.Arithmetic.Integer.get_int expr
                            |> OurInt.of_int
                          else
                            expr
                            |> Z3.Arithmetic.Real.get_ratio
                            (* TODO Round shouldnt be the solution, but do we need this anyway, since we ignore the values of helper variables? *)
                            |> Ratio.round_ratio
                        )
                      in
                      (var, value)
                    )
             )
        |? []
        |> Valuation.from
      else Valuation.from []
      
  end
