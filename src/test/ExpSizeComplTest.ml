
open Batteries
open ProgramTypes
open Polynomials
open OUnit2
open TestHelper
open BoundsInst

let tests =
    "CostComplexity" >:::
      List.map
        (fun (name, var, exp_complexity,prog_path) ->
          name >:::
            [OUnitTest.TestCase (OUnitTest.Custom_length 120., (fun _ ->
              let cache = CacheManager.new_cache () in
              let prog = Readers.read_file (CacheManager.trans_id_counter cache) ("../../" ^ prog_path) in
              let (processed_prog,approx) =
                (prog, Approximation.create prog )
                |> Preprocessor.process (CacheManager.trans_id_counter cache)
                    Preprocessor.process_til_fixpoint
                    Preprocessor.[InvariantGeneration; CutUnsatisfiableTransitions; CutUnreachableLocations; CutZeroProbTransitions; ProbabilityLessOne]
                |> (fun (p,appr) -> Bounds.find_exp_bounds false ~refined_smt_timeout:(Some 5.0) ~generate_invariants_bottom_up:Preprocessor.generate_invariants true cache p appr)
              in
              let expsize_bound = Approximation.program_expsizebound approx processed_prog var in
              let expsize_compl = expsize_bound |> RealBound.asymptotic_complexity in
              let error_str =
                  "Mismatch: Expected " ^ (RealBound.show_complexity exp_complexity) ^ " got " ^ (RealBound.to_string expsize_bound)
                  ^ " for prog\n" ^ (Program.to_string ~show_gtcost:true processed_prog)
              in
              assert_bool error_str RealBound.(equal_complexity exp_complexity expsize_compl)
          ))]
        )
        [
          ("pol06", Var.of_string "Z", RealBound.Inf, "examples/ProbabilisticExamples/paper/pol06.koat");
          ("trader", Var.of_string "Z", RealBound.Inf, "examples/ProbabilisticExamples/paper/trader.koat");
        ];
