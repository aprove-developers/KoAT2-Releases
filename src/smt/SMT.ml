open Batteries

(** Provides different implementations of SMT solvers *)
   
(** A unified interface for SMT solvers (currently supported: Z3) *)
module type Solver =
  sig
    module Polynomial_ : PolyTypes.Polynomial
    module Constraint_ : ConstraintTypes.Constraint with module Polynomial_ = Polynomial_
    module Formula_ : ConstraintTypes.Formula with module Polynomial_ = Polynomial_

    val satisfiable : Formula_.t -> bool
    
    val get_model : Formula_.t -> Polynomial_.Valuation_.t
    
    (*val to_string : Constraint.t -> string*)
  end

(** Constructs an SMT solver which uses the microsoft project Z3 *)
module MakeZ3Solver(P : PolyTypes.Polynomial) : (Solver with module Polynomial_ = P
                                                         and module Constraint_ = Constraints.Make(P)
                                                         and module Formula_ = Formula.Make(P)) =
  struct
    module Polynomial_ = P
    module Constraint_ = Constraints.Make(P)
    module Formula_ = Formula.Make(P)
       
    let context = ref (
                      Z3.mk_context [
                          ("model", "true");
                          ("proof", "false");
                        ]
                    )

    let from_constraint (constraints : Formula_.t) =
      Formula_.fold ~const:(fun value -> Z3.Arithmetic.Integer.mk_numeral_i !context (Polynomial_.Value.to_int value))
                      ~var:(fun var ->  if (Polynomial_.Var.is_helper var) then 
                                            Z3.Arithmetic.Real.mk_const_s !context (Polynomial_.Var.to_string var) 
                                        else
                                            Z3.Arithmetic.Integer.mk_const_s !context (Polynomial_.Var.to_string var))
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

        
    (** checks if there exists a satisfying assignment for a given constraint
        uses Z3 optimisation methods*)
    let satisfiable (constraints : Formula_.t) =
        let formula = from_constraint constraints in
        let optimisation_goal = Z3.Optimize.mk_opt !context in
        Z3.Optimize.add optimisation_goal [formula];
        (Z3.Optimize.check optimisation_goal) == Z3.Solver.SATISFIABLE
      
    let get_model (constraints : Formula_.t) =
        let formula = from_constraint constraints in
        let optimisation_goal = Z3.Optimize.mk_opt !context in
            Z3.Optimize.add optimisation_goal [formula];
            let status = Z3.Optimize.check optimisation_goal in
                if (status == Z3.Solver.SATISFIABLE) then
                    let model = Z3.Optimize.get_model optimisation_goal in
                        match model with
                        | None -> Polynomial_.Valuation_.from []
                        | Some model -> 
                            let assigned_values = Z3.Model.get_const_decls model in
                                Polynomial_.Valuation_.from 
                                (List.map 
                                    (fun func_decl -> 
                                        let name = Z3.Symbol.get_string (Z3.FuncDecl.get_name func_decl) in
                                        let var_of_name = (Polynomial_.Var.of_string name) in
                                        let value = Option.get (Z3.Model.get_const_interp
                                        model func_decl)(*careful, this returns an option*) in
                                        let int_of_value = int_of_float (float_of_string (Z3.Expr.to_string value)) in (*dirty hack, might be solved better*)
                                        let value_of_value = (Polynomial_.Value.of_int int_of_value) in
                                            (var_of_name,value_of_value)) assigned_values)
                                        
                else Polynomial_.Valuation_.from []
            
(*match Solver.get_model solver with
          | Some model ->
            let consts = Model.get_const_decls model in
            Some (List.fold_left
                    (fun assignment func_decl ->
                      let name = Symbol.get_string (FuncDecl.get_name func_decl) in
                      let value = Utils.unboxOption (Model.get_const_interp model func_decl) in
                      let value_string = Str.replace_first neg_re "-\\1" (Expr.to_string value) in
                      VarMap.add (Poly.mkVar name) (Big_int.big_int_of_string value_string) assignment)
                    VarMap.empty
                    consts)
          | _ -> assert (false) (* SAT but no model! Oh noes! *)*)
        
  end
