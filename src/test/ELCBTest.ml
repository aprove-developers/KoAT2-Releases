open Batteries
open ProgramTypes
open Formulas
open BoundsInst
open OUnit2
open TestHelper

type concave_convex = Convex | Concave | None [@@deriving show]

let tests =
  let varx = RealBound.of_var @@ Var.mk_arg 0 in
  let vary = RealBound.of_var @@ Var.mk_arg 1 in
  "ELSB" >::: [
    "elcb" >::: List.map
      (fun (gt,argnum,l,lower_bound, lower_bound_red, program_string) ->
        (Int.to_string gt) >::
          (fun _ ->
            let var = Var.mk_arg argnum in
            let cache = CacheManager.new_cache () in
            let elcb_cache = CacheManager.elcb_cache cache in
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
            let loc =
                Program.graph prog |> TransitionGraph.locations
                |> LocationSet.filter ((=) l % Location.to_string) |> LocationSet.any
            in
            let rv = ((gt,loc),var) in
            let elcb     = ExpLocalChangeBound.(elcb elcb_cache rv) in
            let elcb_red = ExpLocalChangeBound.(reduced_elcb elcb_cache rv) in
            let error_string =
              "elcb_mismatch elcb: " ^ (RealBound.show ~complexity:false elcb)
              ^ " expected " ^ (RealBound.show ~complexity:false lower_bound)
              ^ " in program " ^ (Program.to_string prog)
            in
            let error_string_red =
              "elcb_mismatch reduced_elcb: " ^ (RealBound.show ~complexity:false elcb_red)
              ^ " expected " ^ (RealBound.show ~complexity:false lower_bound_red)
              ^ " in program " ^ (Program.to_string prog)
            in
            assert_bool error_string (bounds_pos_vars lower_bound elcb);
            assert_bool error_string_red (bounds_pos_vars lower_bound_red elcb_red)
          )
      )
      [
        (0, 0, "g", RealBound.zero, RealBound.zero,
          "(GOAL EXPECTEDCOMPLEXITY)            \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))      \n"
        ^ "(VAR X)                              \n"
        ^ "(RULES                               \n"
        ^ "  f(X) -> 0.5:g(X) :+: 0.5:g(X)      \n"
        ^ ")                                    \n"
        );

        (0, 0, "g", RealBound.zero, RealBound.zero,
          "(GOAL EXPECTEDCOMPLEXITY)            \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))      \n"
        ^ "(VAR X)                              \n"
        ^ "(RULES                               \n"
        ^ "  f(X) -> 0.5:g(X) :+: 0.5:h(X)      \n"
        ^ ")                                    \n"
        );

        (0, 0, "g", RealBound.zero, RealBound.zero,
          "(GOAL EXPECTEDCOMPLEXITY)            \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))      \n"
        ^ "(VAR X Y)                            \n"
        ^ "(RULES                               \n"
        ^ "  f(X,Y) -> g(X,2*Y)                 \n"
        ^ ")                                    \n"
        );

        (0, 1, "g", RealBound.(abs @@ vary), RealBound.(abs @@ vary),
          "(GOAL EXPECTEDCOMPLEXITY)            \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))      \n"
        ^ "(VAR X Y)                            \n"
        ^ "(RULES                               \n"
        ^ "  f(X,Y) -> g(X,2*Y)                 \n"
        ^ ")                                    \n"
        );

        (0, 1, "g", RealBound.(abs @@ vary), RealBound.(abs @@ vary),
          "(GOAL EXPECTEDCOMPLEXITY)                  \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))            \n"
        ^ "(VAR X Y)                                  \n"
        ^ "(RULES                                     \n"
        ^ "  f(X,Y) -> 0.5:g(X,2*Y) :+: 0.5:g(X,2*Y)  \n"
        ^ ")                                          \n"
        );

        (0, 1, "g", RealBound.(of_constant (OurFloat.of_float 0.5) * abs vary), RealBound.(abs @@ vary),
          "(GOAL EXPECTEDCOMPLEXITY)                  \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))            \n"
        ^ "(VAR X Y)                                  \n"
        ^ "(RULES                                     \n"
        ^ "  f(X,Y) -> 0.5:g(X,2*Y)  :+: 0.5:h(X,2*Y) \n"
        ^ ")                                          \n"
        );

        (0, 1, "g", RealBound.(abs @@ vary - (of_constant @@ OurFloat.of_int 5) * varx), RealBound.(abs @@ vary - (of_constant @@ OurFloat.of_int 5) * varx),
          "(GOAL EXPECTEDCOMPLEXITY)            \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))      \n"
        ^ "(VAR X Y)                            \n"
        ^ "(RULES                               \n"
        ^ "  f(X,Y) -> g(X,2*Y - 5*X)           \n"
        ^ ")                                    \n"
        );

        (0, 0, "g", RealBound.infinity, RealBound.infinity,
          "(GOAL EXPECTEDCOMPLEXITY)            \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))      \n"
        ^ "(VAR X)                              \n"
        ^ "(RULES                               \n"
        ^ "  f(X) -> g(X+Z)                     \n"
        ^ ")                                    \n"
        );

        (0, 0, "g", RealBound.(of_constant (OurFloat.of_int 5)), RealBound.(of_constant (OurFloat.of_int 5)),
          "(GOAL EXPECTEDCOMPLEXITY)            \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))      \n"
        ^ "(VAR X)                              \n"
        ^ "(RULES                               \n"
        ^ "  f(X) -> g(X+Z) :|: Z=5             \n"
        ^ ")                                    \n"
        );

        (0, 0, "g", RealBound.(of_constant (OurFloat.of_int 10)), RealBound.(of_constant (OurFloat.of_int 10)),
          "(GOAL EXPECTEDCOMPLEXITY)            \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))      \n"
        ^ "(VAR X)                              \n"
        ^ "(RULES                               \n"
        ^ "  f(X) -> g(X+2*Z) :|: Z=5           \n"
        ^ ")                                    \n"
        );

        (0, 0, "g", RealBound.(of_constant (OurFloat.of_int 25)), RealBound.(of_constant (OurFloat.of_int 25)),
          "(GOAL EXPECTEDCOMPLEXITY)            \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))      \n"
        ^ "(VAR X)                              \n"
        ^ "(RULES                               \n"
        ^ "  f(X) -> g(X+Z*Z) :|: Z=5           \n"
        ^ ")                                    \n"
        );

        (0, 0, "g", RealBound.(of_constant (OurFloat.of_int 5)), RealBound.(of_constant (OurFloat.of_int 5)),
          "(GOAL EXPECTEDCOMPLEXITY)            \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))      \n"
        ^ "(VAR X)                              \n"
        ^ "(RULES                               \n"
        ^ "  f(X) -> g(X+Z) :|: Z<=5 && Z>=0    \n"
        ^ ")                                    \n"
        );

        (0, 0, "g", RealBound.(abs @@ varx + of_constant (OurFloat.of_int 5)), RealBound.(abs @@ varx + of_constant (OurFloat.of_int 5)),
          "(GOAL EXPECTEDCOMPLEXITY)            \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))      \n"
        ^ "(VAR X)                              \n"
        ^ "(RULES                               \n"
        ^ "  f(X) -> g(Z) :|: Z<=5 && Z>=0      \n"
        ^ ")                                    \n"
        );

        (0, 0, "g", RealBound.(of_constant (OurFloat.of_int 50)), RealBound.(of_constant (OurFloat.of_int 50)),
          "(GOAL EXPECTEDCOMPLEXITY)                   \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))             \n"
        ^ "(VAR X)                                     \n"
        ^ "(RULES                                      \n"
        ^ "  f(X) -> g(X+Z*Y) :|: Z<=5 && Z>=0 && Y=10 \n"
        ^ ")                                           \n"
        );

        (0, 0, "g", RealBound.(of_constant @@ OurFloat.of_int 100), RealBound.(of_constant @@ OurFloat.of_int 100),
          "(GOAL EXPECTEDCOMPLEXITY)                     \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))               \n"
        ^ "(VAR X)                                       \n"
        ^ "(RULES                                        \n"
        ^ "  f(X) -> g(X+Z*Y) :|: Z<=5 && Z>=-10 && Y=10 \n"
        ^ ")                                             \n"
        );

        (0, 0, "g", RealBound.zero, RealBound.zero,
          "(GOAL EXPECTEDCOMPLEXITY)            \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))      \n"
        ^ "(VAR X)                              \n"
        ^ "(RULES                               \n"
        ^ "  f(X) -> 0.5:g(X) :+: 0.5:h(3*X)    \n"
        ^ ")                                    \n"
        );

        (0, 0, "h", RealBound.(abs varx), RealBound.(abs varx),
          "(GOAL EXPECTEDCOMPLEXITY)            \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))      \n"
        ^ "(VAR X)                              \n"
        ^ "(RULES                               \n"
        ^ "  f(X) -> 0.5:g(X) :+: 0.5:h(3*X)    \n"
        ^ ")                                    \n"
        );

        (0, 0, "g", RealBound.(of_constant (OurFloat.of_float 1.5) * abs varx), RealBound.(of_constant (OurFloat.of_float 1.5) * abs varx),
          "(GOAL EXPECTEDCOMPLEXITY)            \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))      \n"
        ^ "(VAR X)                              \n"
        ^ "(RULES                               \n"
        ^ "  f(X) -> 0.5:g(0) :+: 0.5:g(3*X)    \n"
        ^ ")                                    \n"
        );
      ];
    "concave_convex_check" >::: List.map
      (
        fun (bstr,conc_conv) ->
          let cache = CacheManager.new_cache () in
          let elcb_cache = CacheManager.elcb_cache cache in
          let bound = Readers.read_bound bstr |> RealBound.of_intbound in
          let (valid,err_string) =
            match conc_conv with
            | Concave ->
              (ExpLocalChangeBound.bound_is_concave elcb_cache bound, "Bound " ^ bstr ^ " is not concave as expected!")
            | Convex ->
              (ExpLocalChangeBound.bound_is_convex elcb_cache bound, "Bound " ^ bstr ^ " is not convex as expected!")
            | None    ->
              match (ExpLocalChangeBound.bound_is_concave elcb_cache bound, ExpLocalChangeBound.bound_is_convex elcb_cache bound) with
              | (true,true)   -> (false, "Bound " ^ bstr ^ "is convex and concave!")
              | (true,false)  -> (false, "Bound " ^ bstr ^ "is concave!")
              | (false,true)  -> (false, "Bound " ^ bstr ^ "is convex!")
              | (false,false) -> (true, "")

          in
          "" >:: fun _ -> assert_bool err_string valid
      )
      [
        ("|A|", Concave);
        ("A", Concave);
        ("|A|", Convex);

        ("|A| + |B|", Concave);
        ("A + B", Concave);
        ("|A| + |B|", Convex);
        ("A + B", Convex);

        ("|A| + |B| + |C|", Concave);
        ("A + B + C", Concave);
        ("|A| + |B| + |C|", Convex);
        ("A + B + C", Convex);

        ("|A| + |B| + |C| + |D|", Concave);
        ("A + B + C + D", Concave);
        ("|A| + |B| + |C| + |D|", Convex);
        ("A + B + C + D", Convex);

        ("|A| * |B|", None);
        ("A * B", None);

        ("|A| * |B| * |C|", None);
        ("A * B * C", None);

        ("max {1,|A|}", Convex);
        ("max {1,A}", Convex);
        ("min {1,|A|}", Concave);
        ("min {1,A}", Concave);

        ("max {|A|,|B|}", Convex);
        ("max {A,B}", Convex);
        ("min {|A|,|B|}", Concave);
        ("min {A,B}", Concave);

        ("max {1,|A|,|B|}", Convex);
        ("max {1,A,B}", Convex);
        ("min {1,|A|,|B|}", Concave);
        ("min {1,A,B}", Concave);

        ("max {1,|A|,|B|, |A|*|B|}", None);
        ("max {1,A,B, A*B}", None);
        ("min {1,|A|,|B|, |A|*|B|}", None);
        ("min {1,A,B, A*B}", None);
      ]
  ]
