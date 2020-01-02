open Batteries
open ProgramTypes
open Polynomials
open OUnit2
open TestHelper
open BoundsInst

let tests =
  let varx = RealBound.of_var @@ Var.of_string "X" in
  "ExpTimeBound" >::: [
    "ExactBound" >:::
      List.map
        (fun (name,gt_id,lower_bound,prog_path) ->
          let cache = CacheManager.new_cache () in
          let prog = Readers.read_file (CacheManager.trans_id_counter cache) ("../../" ^ prog_path) in
          let gtset = Program.generalized_transitions prog in
          let gt = GeneralTransitionSet.any @@ GeneralTransitionSet.filter ((=) gt_id % GeneralTransition.id) gtset in
          let approx = Approximation.create prog |> Bounds.find_exp_bounds ~generate_invariants:Preprocessor.generate_invariants true cache prog |> Tuple2.second in
          let exptime = Approximation.exptimebound approx gt in
          let error_str =
              "Mismatch: Expected " ^ (RealBound.to_string lower_bound) ^ " got " ^ (RealBound.to_string exptime)
              ^ " for gt " ^ (GeneralTransition.to_id_string gt) ^ " in prog\n" ^ (Program.to_string ~show_gtcost:true prog)
          in
          name >:: (fun _ -> assert_bool error_str (bounds lower_bound exptime) )
        )
        [
          ("trivial_loop", 0, RealBound.(of_constant (OurFloat.of_int 1)), "examples/ProbabilisticExamples/trivial_loop.koat");
          ("trivial_loop", 1, RealBound.(of_constant (OurFloat.of_int 2) * (abs varx)), "examples/ProbabilisticExamples/trivial_loop.koat");
          ("trivial_loop2", 0, RealBound.(of_constant OurFloat.(of_int 1)), "examples/ProbabilisticExamples/trivial_loop2.koat");
          ("trivial_loop2", 1, RealBound.(of_constant OurFloat.(of_int 2 / of_int 5) * (abs varx) + of_constant OurFloat.(of_int 8 / of_int 5)), "examples/ProbabilisticExamples/trivial_loop2.koat");

          ("simple_quadratic", 0, RealBound.(of_constant (OurFloat.of_int 1)), "examples/ProbabilisticExamples/simple_quadratic.koat");
          ("simple_quadratic", 1, RealBound.(of_constant (OurFloat.of_int 2) * (abs varx)), "examples/ProbabilisticExamples/simple_quadratic.koat");
          ("simple_quadratic", 2, RealBound.(of_constant (OurFloat.of_int 1)), "examples/ProbabilisticExamples/simple_quadratic.koat");
          ("simple_quadratic", 3, RealBound.(of_constant (OurFloat.of_int 2) * (abs varx) * (abs varx)), "examples/ProbabilisticExamples/simple_quadratic.koat");
        ];

    "Complexity" >:::
      List.map
        (fun (name,gt_id,exp_complexity,prog_path) ->
          let cache = CacheManager.new_cache () in
          let prog = Readers.read_file (CacheManager.trans_id_counter cache) ("../../" ^ prog_path) in
          let gtset = Program.generalized_transitions prog in
          let gt = GeneralTransitionSet.any @@ GeneralTransitionSet.filter ((=) gt_id % GeneralTransition.id) gtset in
          let approx = Approximation.create prog |> Bounds.find_exp_bounds ~generate_invariants:Preprocessor.generate_invariants true cache prog |> Tuple2.second in
          let exptime_compl = Approximation.exptimebound approx gt |> RealBound.asymptotic_complexity in
          let error_str =
              "Mismatch: Expected " ^ (RealBound.show_complexity exp_complexity) ^ " got " ^ (RealBound.show_complexity exptime_compl)
              ^ " for gt " ^ (GeneralTransition.to_id_string gt) ^ " in prog\n" ^ (Program.to_string ~show_gtcost:true prog)
          in
          name >:: (fun _ -> assert_bool error_str RealBound.(equal_complexity exp_complexity exptime_compl) )
        )
        [
          ("fib_exp_size", 3, RealBound.Exponential 1, "examples/ProbabilisticExamples/paper/fib_exp_size.koat");
        ];
  ]
