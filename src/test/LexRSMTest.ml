open Batteries
open ProgramTypes
open Polynomials
open OUnit2

let tests =
  let varx = RealPolynomial.of_var @@ Var.of_string "X" in
  let vary = RealPolynomial.of_var @@ Var.of_string "Y" in
  "LexRSM" >::: [(
    "exists" >::: (List.map
      (fun (name,l,gt_id,exp,prog_path) ->
        name >::
          (fun _ ->
            let cache = CacheManager.new_cache () in
            let lrsm_cache = CacheManager.lrsm_cache cache in

            let prog = Readers.read_file (CacheManager.trans_id_counter cache) ("../../" ^ prog_path) in
            let gtset = Program.generalized_transitions prog in
            let decr = GeneralTransitionSet.any @@ GeneralTransitionSet.filter ((=) gt_id % GeneralTransition.id) gtset in
            let rankfunc = LexRSM.find lrsm_cache prog decr |> Option.get |> LexRSM.rank in
            let loc =
                Program.graph prog |> TransitionGraph.locations
                |> LocationSet.filter ((=) l % Location.to_string) |> LocationSet.any
            in
            let rank = rankfunc loc in
            let error_str =
                "Mismatch: Expected " ^ (RealPolynomial.to_string exp) ^ " got " ^ (RealPolynomial.to_string rank)
                ^ " with decr " ^ (GeneralTransition.to_id_string decr) ^ " and loc " ^ (Location.to_string loc) ^ " in prog\n" ^ (Program.to_string ~show_gtcost:true prog)
            in
            assert_bool error_str (RealPolynomial.(rank =~= exp))
          )
      )
      [
        ("trivial_loop", "g", 1, RealPolynomial.(of_constant (OurFloat.of_int 2) * varx), "examples/ProbabilisticExamples/trivial_loop.koat");
        ("trivial_loop2", "g", 1, RealPolynomial.(of_constant OurFloat.(of_int 2 / of_int 5) * varx + of_constant OurFloat.(of_int 8 / of_int 5)),
         "examples/ProbabilisticExamples/trivial_loop2.koat");
        ("simple_quadratic_loc_g", "g", 1, RealPolynomial.(of_constant (OurFloat.of_int 2) * varx), "examples/ProbabilisticExamples/simple_quadratic.koat");
        ("simple_quadratic_loc_h", "h", 3, vary, "examples/ProbabilisticExamples/simple_quadratic.koat");
        ("double_trivial_loop_decr_1", "g", 1, RealPolynomial.(varx + of_constant (OurFloat.(one / of_int 2))), "examples/ProbabilisticExamples/double_trivial_loop.koat");
        ("double_trivial_loop_decr_2", "g", 2, varx, "examples/ProbabilisticExamples/double_trivial_loop.koat");
      ]));

    "does_not_exist" >::: (List.map
      (fun (name,gt_id,prog_path) ->
        let cache = CacheManager.new_cache () in
        let lrsm_cache = CacheManager.lrsm_cache cache in

        let prog = Readers.read_file (CacheManager.trans_id_counter cache) ("../../" ^ prog_path) in
        let gtset = Program.generalized_transitions prog in
        let decr = GeneralTransitionSet.any @@ GeneralTransitionSet.filter ((=) gt_id % GeneralTransition.id) gtset in
        let rank = LexRSM.find lrsm_cache prog decr in
        let error_str r =
          "Found LexRSM for decr: " ^ (GeneralTransition.to_id_string decr) ^ " with " ^ (LexRSM.pprf_to_string r)
          ^ " where none should exist. Program: \n" ^ (Program.to_string ~show_gtcost:true prog)
        in
        name >:: (fun _ -> if Option.is_some rank then assert_string (error_str @@ Option.get rank) else ())
      )
      [
        ("trivial_loop", 0, "examples/ProbabilisticExamples/trivial_loop.koat");
        ("trivial_loop2", 0, "examples/ProbabilisticExamples/trivial_loop2.koat");
        ("simple_quadratic_gt_0", 0, "examples/ProbabilisticExamples/simple_quadratic.koat");
        ("simple_quadratic_gt_2", 2, "examples/ProbabilisticExamples/simple_quadratic.koat");
        ("double_trivial_loop", 0, "examples/ProbabilisticExamples/double_trivial_loop.koat");
        ("increasing_loop", 1, "examples/ProbabilisticExamples/NoLexRSM/increasing_loop.koat");
        ("increasing_nondet_loop", 1, "examples/ProbabilisticExamples/NoLexRSM/increasing_nondet_loop.koat");
        ("exp_increase", 1, "examples/ProbabilisticExamples/NoLexRSM/exp_increase.koat");
      ])

      ]