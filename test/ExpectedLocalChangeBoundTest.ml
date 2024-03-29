open Koat2
open! OurBase
open OUnit2
open ProbabilisticProgramModules
open Bounds

let tests =
  let var_first_arg = Sequence.hd_exn Var.args in
  let bound_first_arg = RationalBound.of_var var_first_arg in
  let loc_g = Location.of_string "g" in
  let one_half = RationalBound.of_constant (OurRational.of_float 0.5) in
  "ExpectedChangeBoundTests"
  >::: List.mapi
         ~f:(fun i (gt, expected) ->
           Printf.sprintf "case %i:%s" i gt >:: fun _ ->
           let gt = Set.choose_exn (Readers.read_general_transitions gt) in
           let program_vars = GeneralTransition.input_vars gt in
           let elcb = ExpectedLocalChangeBound.compute_elcb program_vars ((gt, loc_g), var_first_arg) in

           Helper.assert_equal_rationalpoly_smt
             (Option.value_exn @@ RationalBound.to_poly expected)
             (Option.value_exn @@ RationalBound.to_poly elcb))
         RationalBound.
           [
             ("(RULES f(X) -> g(X))", zero);
             ("(RULES f(X) -> 0.5:g(X) :+: 0.5:g(X) )", zero);
             ("(RULES f(X) -> 0.5:g(X) :+: 0.5:h(X) )", zero);
             ("(RULES f(X) -> g(2*X) )", bound_first_arg);
             ("(RULES f(X) -> 0.5:g(2*X) :+: 0.5:h(X) )", one_half * bound_first_arg);
             ("(RULES f(X) -> g(UNIFORM(0,X)) )", zero);
             ( "(RULES f(X) -> g(UNIFORM(0,2*X)) )",
               (one + one) * bound_first_arg (* a more precise bound like 1*X can not be computed atm *) );
             ("(RULES f(X) -> 0.5:g(UNIFORM(0,X)) :+: 0.5:h(X))", zero);
             ("(RULES f(X) -> 0.5:g(UNIFORM(0,2*X)) :+: 0.5:h(X))", bound_first_arg);
             ("(RULES f(X) -> g(UNIFORM(X,2*X)) )", one_half * bound_first_arg);
             ("(RULES f(X) -> 0.5:g(UNIFORM(X,2*X)) :+: 0.5:h(X))", one_half * one_half * bound_first_arg);
             ("(RULES g(X,Y) -> 0.5:g(X-1,Y+X) :+: 0.5:g(X,Y+X) :|: X>0) )", zero);
             ( "(RULES g(X,Y) -> 0.5:g(X+Y,Y-1) :+: 0.5:g(X+Y,Y) :|: Y>0) )",
               of_var (Sequence.nth_exn Var.args 1) );
             ("(RULES f(X) -> g(UNIFORM(X,X+1)) )", one_half);
             ("(RULES f(X) -> 0.5:g(X+1) :+: 0.5:g(X-1))", one);
             ("(RULES f(X) -> g(UNIFORM(X-1,X+1)))", one);
             ("(RULES f(X) -> g(Y) :|: Y = X)", zero)
             (* currently we only use local size bounds to limit the absolute value of temporary variables but we cannot reason about their precise values *);
             ("(RULES f(X) -> g(Y+1) :|: Y = X)", ((one + one) * bound_first_arg) + one);
             ("(RULES f(X) -> g(X+Y) :|: Y = 1)", one);
             ("(RULES f(X) -> g(X+Y) :|: -1 <= Y && Y <= 1)", one);
             ("(RULES f(X) -> g(X*Y) :|: -1 <= Y && Y <= 1)", zero);
             ("(RULES f(X) -> g(X*Y) :|: -1 <= Y && Y <= 2)", (one + one + one) * bound_first_arg);
             ("(RULES f(X) -> g(UNIFORM(X,X+Y)) :|: Y = X)", one_half * bound_first_arg);
             ("(RULES f(X) -> g(UNIFORM(X-Y,X)) :|: Y = X)", zero);
             ("(RULES f(X) -> g(UNIFORM(X-Y,X+Y)) :|: Y = X)", bound_first_arg);
             ( "(RULES f(X) -> g(UNIFORM(X-1,X+2)*UNIFORM(-3,4)) :|: Y = X)",
               (of_constant (OurRational.of_float 4.5) * bound_first_arg)
               + of_constant (OurRational.of_float 5.25) )
             (* imprecise due to abs overestimation of unif distributions*);
             ("(RULES f(X) -> g(X + BINOMIAL(2,0.5)^2) )", of_constant (OurRational.of_float 1.5));
           ]
