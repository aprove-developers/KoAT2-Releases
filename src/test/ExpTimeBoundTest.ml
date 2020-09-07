open Batteries
open ProgramTypes
open Polynomials
open OUnit2
open TestHelper
open BoundsInst

let tests =
  let varx = RealBound.of_var @@ Var.mk_arg 0 in
  "ExpTimeBound" >::: [
    "ExactBound" >:::
      List.map
        (fun (name,gt_id,lower_bound,prog_path) ->
          name >:: (fun _ ->
            let cache = CacheManager.new_cache () in
            let prog = Readers.read_file (CacheManager.trans_id_counter cache) ("../../" ^ prog_path) in
            let gtset = Program.generalized_transitions prog in
            let gt = GeneralTransitionSet.any @@ GeneralTransitionSet.filter ((=) gt_id % GeneralTransition.id) gtset in
            let approx = Approximation.create prog |> Bounds.find_exp_bounds false ~refined_smt_timeout:(Some 1.0) ~generate_invariants_bottom_up:Preprocessor.generate_invariants true cache prog |> Tuple2.second in
            let exptime = Approximation.exptimebound approx gt in
            let error_str =
                "Mismatch: Expected " ^ (RealBound.to_string lower_bound) ^ " got " ^ (RealBound.to_string exptime)
                ^ " for gt " ^ (GeneralTransition.to_id_string gt) ^ " in prog\n" ^ (Program.to_string prog)
            in
            assert_bool error_str (bounds_pos_vars lower_bound exptime)
          )
        )
        [
          ("trivial_loop", 0, RealBound.(of_constant (OurFloat.of_int 1)), "examples/ProbabilisticExamples/trivial_loop.koat");
          ("trivial_loop", 1, RealBound.(of_constant (OurFloat.of_int 2) * varx), "examples/ProbabilisticExamples/trivial_loop.koat");
          ("trivial_loop2", 0, RealBound.(of_constant OurFloat.(of_int 1)), "examples/ProbabilisticExamples/trivial_loop2.koat");
          ("trivial_loop2", 1, RealBound.(of_constant OurFloat.(of_int 2 / of_int 5) * varx + of_constant OurFloat.(of_int 8 / of_int 5)), "examples/ProbabilisticExamples/trivial_loop2.koat");

          ("simple_quadratic", 0, RealBound.(of_constant (OurFloat.of_int 1)), "examples/ProbabilisticExamples/simple_quadratic.koat");
          ("simple_quadratic", 1, RealBound.(of_constant (OurFloat.of_int 2) * varx), "examples/ProbabilisticExamples/simple_quadratic.koat");
          ("simple_quadratic", 2, RealBound.(of_constant (OurFloat.of_int 1)), "examples/ProbabilisticExamples/simple_quadratic.koat");
          ("simple_quadratic", 3, RealBound.(of_constant (OurFloat.of_int 2) * varx * varx), "examples/ProbabilisticExamples/simple_quadratic.koat");
        ];

    "CostComplexity" >:::
      List.map
        (fun (name,exp_complexity,prog_path) ->
          name >:::
            [OUnitTest.TestCase (OUnitTest.Custom_length 1200., (fun _ ->
              let cache = CacheManager.new_cache () in
              let prog = Readers.read_file (CacheManager.trans_id_counter cache) ("../../" ^ prog_path) in
              let (processed_prog,approx) =
                (prog, Approximation.create prog )
                |> Preprocessor.process (CacheManager.trans_id_counter cache)
                    Preprocessor.process_til_fixpoint
                    Preprocessor.[InvariantGeneration; CutUnsatisfiableTransitions; CutUnreachableLocations; CutZeroProbTransitions; Chaining]
                |> (fun (p,appr) -> Bounds.find_exp_bounds false ~refined_smt_timeout:(Some 5.0) ~generate_invariants_bottom_up:Preprocessor.generate_invariants true cache p appr)
              in
              let expcost_bound = Approximation.program_expcostbound approx processed_prog in
              let expcost_compl = expcost_bound |> RealBound.asymptotic_complexity in
              let error_str =
                  "Mismatch: Expected " ^ (RealBound.show_complexity exp_complexity) ^ " got " ^ (RealBound.to_string expcost_bound)
                  ^ " for prog\n" ^ (Program.to_string processed_prog)
              in
              assert_bool error_str RealBound.(equal_complexity exp_complexity expcost_compl)
          ))]
        )
        [
          ("2drwalk", RealBound.Polynomial 1, "examples/ProbabilisticExamples/paper/2drwalk.koat");
          ("alain.c", RealBound.Polynomial 3, "examples/ProbabilisticExamples/paper/alain.c.koat");
          ("ber", RealBound.Polynomial 1, "examples/ProbabilisticExamples/paper/ber.koat");
          ("bin", RealBound.Polynomial 1, "examples/ProbabilisticExamples/paper/bin.koat");
          ("c4b_t09", RealBound.Polynomial 1, "examples/ProbabilisticExamples/paper/c4b_t09.koat");
          ("c4b_t13", RealBound.Polynomial 1, "examples/ProbabilisticExamples/paper/c4b_t13.koat");
          ("c4b_t15", RealBound.Polynomial 1, "examples/ProbabilisticExamples/paper/c4b_t15.koat");
          ("c4b_t19", RealBound.Polynomial 1, "examples/ProbabilisticExamples/paper/c4b_t19.koat");
          ("c4b_t30", RealBound.Polynomial 1, "examples/ProbabilisticExamples/paper/c4b_t30.koat");
          ("c4b_t61", RealBound.Polynomial 1, "examples/ProbabilisticExamples/paper/c4b_t61.koat");
          ("complex", RealBound.Polynomial 2, "examples/ProbabilisticExamples/paper/complex.koat");
          ("condand", RealBound.Polynomial 1, "examples/ProbabilisticExamples/paper/condand.koat");
          ("cooling", RealBound.Polynomial 1, "examples/ProbabilisticExamples/paper/cooling.koat");
          ("cousot9", RealBound.Polynomial 2, "examples/ProbabilisticExamples/paper/cousot9.koat");
          ("ex_paper1.c", RealBound.Polynomial 2, "examples/ProbabilisticExamples/paper/ex_paper1.c.koat");
          ("fcall", RealBound.Polynomial 1, "examples/ProbabilisticExamples/paper/fcall.koat");
          ("fib_exp_size", RealBound.Exponential 1, "examples/ProbabilisticExamples/paper/fib_exp_size.koat");
          ("fig1", RealBound.Polynomial 1, "examples/ProbabilisticExamples/paper/fig1.koat");
          ("fig3", RealBound.Polynomial 1, "examples/ProbabilisticExamples/paper/fig1.koat");
          ("fig5", RealBound.Polynomial 0, "examples/ProbabilisticExamples/paper/fig5.koat");
          ("filling_vol", RealBound.Polynomial 1, "examples/ProbabilisticExamples/paper/filling_vol.koat");
          ("hyper", RealBound.Polynomial 1, "examples/ProbabilisticExamples/paper/hyper.koat");
          ("knuth_morris_pratt.c", RealBound.Polynomial 1, "examples/ProbabilisticExamples/paper/knuth_morris_pratt.c.koat");
          ("leading", RealBound.Polynomial 2, "examples/ProbabilisticExamples/paper/leading.koat");
          ("linear01", RealBound.Polynomial 1, "examples/ProbabilisticExamples/paper/linear01.koat");
          ("linear01", RealBound.Polynomial 1, "examples/ProbabilisticExamples/paper/linear01.koat");
          ("multirace", RealBound.Polynomial 2, "examples/ProbabilisticExamples/paper/multirace.koat");
          ("multirace2", RealBound.Polynomial 2, "examples/ProbabilisticExamples/paper/multirace2.koat");
          ("nestedrdwalk", RealBound.Polynomial 1, "examples/ProbabilisticExamples/paper/nested_rdwalk.koat");
          ("no_loop", RealBound.Polynomial 0, "examples/ProbabilisticExamples/paper/no_loop.koat");
          ("pol04", RealBound.Inf, "examples/ProbabilisticExamples/paper/pol04.koat");
          ("pol05", RealBound.Inf, "examples/ProbabilisticExamples/paper/pol05.koat");
          ("pol07", RealBound.Polynomial 2, "examples/ProbabilisticExamples/paper/pol07.koat");
          ("prnes", RealBound.Polynomial 1, "examples/ProbabilisticExamples/paper/prnes.koat");
          ("prnes2", RealBound.Polynomial 1, "examples/ProbabilisticExamples/paper/prnes2.koat");
          ("probloop", RealBound.Polynomial 1, "examples/ProbabilisticExamples/paper/prob_loop.koat");
          ("prseq", RealBound.Polynomial 1, "examples/ProbabilisticExamples/paper/prseq.koat");
          ("prspeed", RealBound.Polynomial 1, "examples/ProbabilisticExamples/paper/prspeed.koat");
          ("race", RealBound.Polynomial 1, "examples/ProbabilisticExamples/paper/race.koat");
          ("rdbub", RealBound.Polynomial 2, "examples/ProbabilisticExamples/paper/rdbub.koat");
          ("rdseql", RealBound.Polynomial 1, "examples/ProbabilisticExamples/paper/rdseql.koat");
          ("rdspeed", RealBound.Polynomial 1, "examples/ProbabilisticExamples/paper/rdspeed.koat");
          ("rdwalk", RealBound.Polynomial 1, "examples/ProbabilisticExamples/paper/rdwalk.koat");
          ("realheapsort", RealBound.Polynomial 2, "examples/ProbabilisticExamples/paper/realheapsort.koat");
          ("rejection_sampling", RealBound.Polynomial 1, "examples/ProbabilisticExamples/paper/rejection_sampling.koat");
          ("robot", RealBound.Polynomial 1, "examples/ProbabilisticExamples/paper/robot.koat");
          ("roulette", RealBound.Polynomial 1, "examples/ProbabilisticExamples/paper/roulette.koat");
          ("SelectSort", RealBound.Polynomial 2, "examples/ProbabilisticExamples/paper/SelectSort.koat");
          ("simple_nested", RealBound.Polynomial 2, "examples/ProbabilisticExamples/paper/simple_nested.koat");
          ("simple_recursive", RealBound.Polynomial 1, "examples/ProbabilisticExamples/paper/simple_recursive.koat");

          (* O(n^1) should also be a valid bound for spctrm, however it is not currently found by KoAT
           * The reason is the transition f66 -> f66. It seems like the LexRSM chooses a big non-increasing set
           * which leads to this quadratic bound *)
          ("spctrm", RealBound.Polynomial 2, "examples/ProbabilisticExamples/paper/spctrm.koat");
          ("sprdwalk", RealBound.Polynomial 1, "examples/ProbabilisticExamples/paper/sprdwalk.koat");
          ("trapped_miner", RealBound.Polynomial 1, "examples/ProbabilisticExamples/paper/trapped_miner.koat");
          ("TruncSelectSort", RealBound.Polynomial 2, "examples/ProbabilisticExamples/paper/TruncSelectSort.koat");
          ("two_arrays2", RealBound.Polynomial 1, "examples/ProbabilisticExamples/paper/two_arrays2.koat");
        ];
  ]
