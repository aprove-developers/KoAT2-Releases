open Batteries
open ProgramTypes
open Formulas
open BoundsInst
open OUnit2
open TestHelper
open RVGTypes

module RV = ERVG.RV

let tests =
  let varx = RealBound.of_var @@ Var.of_string "X" in
  "ELSB" >:::
    List.map
      (fun (gt,v,l,lower_bound, program_string) ->
        let cache = CacheManager.new_cache () in

        let prog = Readers.read_program (CacheManager.trans_id_counter cache) program_string in
        let gt_id_offset =
          Program.generalized_transitions prog
          |> GeneralTransitionSet.to_list
          |> List.map GeneralTransition.id
          |> List.sort compare
          |> List.hd
        in
        let gt =
          Program.generalized_transitions prog
          |> GeneralTransitionSet.filter ((=) (gt + gt_id_offset) % GeneralTransition.id)
          |> GeneralTransitionSet.any
        in
        let var = Var.of_string v in
        let loc =
            Program.graph prog |> TransitionGraph.locations
            |> LocationSet.filter ((=) l % Location.to_string) |> LocationSet.any
        in
        let approx = Approximation.create prog |> Bounds.find_exp_bounds ~generate_invariants:Preprocessor.generate_invariants false cache prog |> Tuple2.second in
        let expsize = Approximation.expsizebound `Upper approx (gt,loc) var in
        let error_string =
          "exptrivsize_mismatch exp_size: " ^ (RealBound.show ~complexity:false expsize)
          ^ " expected " ^ (RealBound.show ~complexity:false lower_bound)
          ^ " in grv" ^ (RV.to_id_string ((gt,loc),var))
          ^ " in program " ^ (Program.to_string ~show_gtcost:true prog)
        in
        (GeneralTransition.to_string gt) >:: fun _ -> assert_bool error_string (bounds_pos_vars lower_bound expsize)
      )
      [
        (0, "X", "g", RealBound.(abs varx),
          "(GOAL EXPECTEDCOMPLEXITY)            \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))      \n"
        ^ "(VAR X)                              \n"
        ^ "(RULES                               \n"
        ^ "  f(X) -> g(X)                       \n"
        ^ ")                                    \n"
        );

        (0, "X", "g", RealBound.((of_constant @@ OurFloat.of_int 2) * (abs varx)),
          "(GOAL EXPECTEDCOMPLEXITY)           \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))     \n"
        ^ "(VAR X)                             \n"
        ^ "(RULES                              \n"
        ^ "  f(X) -> 0.5:g(X) :+: 0.5:g(3*X)   \n"
        ^ "  g(X) -> h(X)                      \n"
        ^ ")                                   \n"
        );

        (1, "X", "h", RealBound.((of_constant @@ OurFloat.of_float 1.5) * (abs varx)),
          "(GOAL EXPECTEDCOMPLEXITY)           \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))     \n"
        ^ "(VAR X)                             \n"
        ^ "(RULES                              \n"
        ^ "  f(X) -> 0.5:g(X) :+: 0.5:g(2*X)   \n"
        ^ "  g(X) -> h(X)                      \n"
        ^ ")                                   \n"
        );

        (3, "X", "j", RealBound.((of_constant @@ OurFloat.of_float 1.5) * (abs varx)),
          "(GOAL EXPECTEDCOMPLEXITY)           \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))     \n"
        ^ "(VAR X)                             \n"
        ^ "(RULES                              \n"
        ^ "  f(X) -> 0.5:g(X) :+: 0.5:h(2*X)   \n"
        ^ "  g(X) -> i(X)                      \n"
        ^ "  h(X) -> i(X)                      \n"
        ^ "  i(X) -> j(X)                      \n"
        ^ ")                                   \n"
        );

        (5, "X", "e", RealBound.((of_constant @@ OurFloat.of_float 3.0) * (abs varx)),
          "(GOAL EXPECTEDCOMPLEXITY)          \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))    \n"
        ^ "(VAR X)                            \n"
        ^ "(RULES                             \n"
        ^ "  f(X) -> 0.5:g(X) :+: 0.5:h(2*X)  \n"
        ^ "  h(X) -> 0.5:a(X) :+: 0.5:b(4*X)  \n"
        ^ "  a(X) -> k(X)                     \n"
        ^ "  b(X) -> k(X)                     \n"
        ^ "  g(X) -> k(X)                     \n"
        ^ "  k(X) -> e(X)                     \n"
        ^ ")                                  \n"

        )

      ]
