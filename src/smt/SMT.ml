open Batteries

(** SMT solver which uses the microsoft project Z3 *)
module Z3Solver =
  struct
    module Valuation = Valuation.Make(OurInt)
    type valuation = Valuation.t
    type formula = Formula.PolynomialFormula.t
       
    let context = ref (
                      Z3.mk_context [
                          ("model", "true");
                          ("proof", "false");
                        ]
                    )

    let from_constraint (constraints : formula) =
      Formula.PolynomialFormula.fold ~const:(fun value -> Z3.Arithmetic.Integer.mk_numeral_i !context (OurInt.to_int value))
                                     ~var:(fun var ->  if (Var.is_helper var) then 
                                                         Z3.Arithmetic.Real.mk_const_s !context (Var.to_string var) 
                                                       else
                                                         Z3.Arithmetic.Integer.mk_const_s !context (Var.to_string var))
                                     ~neg:(Z3.Arithmetic.mk_unary_minus !context)
                                     ~plus:(fun p1 p2 -> Z3.Arithmetic.mk_add !context [p1; p2])
                                     ~times:(fun p1 p2 -> Z3.Arithmetic.mk_mul !context [p1; p2])
                                     ~pow:(fun b e -> Z3.Arithmetic.mk_power !context b (Z3.Arithmetic.Integer.mk_numeral_i !context e))
                                     ~le:(Z3.Arithmetic.mk_le !context)
                                     ~correct:(Z3.Boolean.mk_true !context)
                                     ~conj:(fun a1 a2 -> Z3.Boolean.mk_and !context [a1; a2])
                                     ~wrong:(Z3.Boolean.mk_false !context)
                                     ~disj:(fun a1 a2 -> Z3.Boolean.mk_or !context [a1; a2])
                                     constraints
                      
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
        
    let match_string_for_float str =
        let float_regexp = Str.regexp "(?\\([-]?\\)[ ]?\\([0-9]*\\.?[0-9]+\\))?" in             (*(?\\([-0-9\\.]*\\))?*)
            Str.replace_first float_regexp "\\1\\2" str 
    
    let get_model (constraints : formula) =
        let formula = from_constraint constraints in
        let optimisation_goal = Z3.Optimize.mk_opt !context in
            Z3.Optimize.add optimisation_goal [formula];
            let status = Z3.Optimize.check optimisation_goal in
                if (status == Z3.Solver.SATISFIABLE) then
                    let model = Z3.Optimize.get_model optimisation_goal in
                        match model with
                        | None -> Valuation.from []
                        | Some model -> 
                            let assigned_values = Z3.Model.get_const_decls model in
                                Valuation.from 
                                (List.map 
                                    (fun func_decl -> 
                                        let name = Z3.Symbol.get_string (Z3.FuncDecl.get_name func_decl) in
                                        let var_of_name = (Var.of_string name) in
                                        let value = Option.get (Z3.Model.get_const_interp
                                        model func_decl)(*careful, this returns an option*) in

                                        let int_of_value = Int.of_float (Float.of_string ( match_string_for_float (Z3.Expr.to_string value))) in 
                                        let value_of_value = (OurInt.of_int int_of_value) in
                                            (var_of_name,value_of_value)) assigned_values)
                else Valuation.from []
        
  end
