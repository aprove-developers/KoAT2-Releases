open Batteries
open ProgramTypes
open Formulas
open BoundsInst
open OUnit2
open TestHelper

type concave_convexe = Convexe | Concave | None [@@deriving show]

let tests =
  let varx = RealBound.of_var @@ Var.of_string "X" in
  let vary = RealBound.of_var @@ Var.of_string "Y" in
  "ELSB" >::: [
    "elsb" >::: List.map
      (fun (gt,v,l,lower_bound, program_string) ->
        let prog = Readers.read_program program_string in
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
        let rv = ((gt,loc),var) in
        let elsb = ExpLocalSizeBound.elsb prog rv in
        let error_string =
          "elsb_mismatch elsb: " ^ (RealBound.show ~complexity:false elsb)
          ^ " expected " ^ (RealBound.show ~complexity:false lower_bound)
          ^ " in program " ^ (Program.to_string prog)
        in
        (GeneralTransition.to_string gt) >:: fun _ -> assert_bool error_string (bounds lower_bound elsb)
      )
      [
        (0, "X", "g", RealBound.zero,
          "(GOAL EXPECTEDCOMPLEXITY)            \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))      \n"
        ^ "(VAR X)                              \n"
        ^ "(RULES                               \n"
        ^ "  f(X) -> g(X)                       \n"
        ^ ")                                    \n"
        );

        (0, "X", "g", RealBound.zero,
          "(GOAL EXPECTEDCOMPLEXITY)            \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))      \n"
        ^ "(VAR X Y)                            \n"
        ^ "(RULES                               \n"
        ^ "  f(X,Y) -> g(X,2*Y)                 \n"
        ^ ")                                    \n"
        );

        (0, "Y", "g", RealBound.(abs @@ vary),
          "(GOAL EXPECTEDCOMPLEXITY)            \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))      \n"
        ^ "(VAR X Y)                            \n"
        ^ "(RULES                               \n"
        ^ "  f(X,Y) -> g(X,2*Y)                 \n"
        ^ ")                                    \n"
        );

        (0, "Y", "g", RealBound.(abs @@ vary - (of_constant @@ OurFloat.of_int 5) * varx),
          "(GOAL EXPECTEDCOMPLEXITY)            \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))      \n"
        ^ "(VAR X Y)                            \n"
        ^ "(RULES                               \n"
        ^ "  f(X,Y) -> g(X,2*Y - 5*X)           \n"
        ^ ")                                    \n"
        );

        (0, "X", "g", RealBound.(infinity),
          "(GOAL EXPECTEDCOMPLEXITY)            \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))      \n"
        ^ "(VAR X)                              \n"
        ^ "(RULES                               \n"
        ^ "  f(X) -> g(X+Z)                     \n"
        ^ ")                                    \n"
        );

        (0, "X", "g", RealBound.(of_constant (OurFloat.of_int 5)),
          "(GOAL EXPECTEDCOMPLEXITY)            \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))      \n"
        ^ "(VAR X)                              \n"
        ^ "(RULES                               \n"
        ^ "  f(X) -> g(X+Z) :|: Z=5             \n"
        ^ ")                                    \n"
        );

        (0, "X", "g", RealBound.(of_constant (OurFloat.of_int 10)),
          "(GOAL EXPECTEDCOMPLEXITY)            \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))      \n"
        ^ "(VAR X)                              \n"
        ^ "(RULES                               \n"
        ^ "  f(X) -> g(X+2*Z) :|: Z=5             \n"
        ^ ")                                    \n"
        );

        (0, "X", "g", RealBound.(of_constant (OurFloat.of_int 25)),
          "(GOAL EXPECTEDCOMPLEXITY)            \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))      \n"
        ^ "(VAR X)                              \n"
        ^ "(RULES                               \n"
        ^ "  f(X) -> g(X+Z*Z) :|: Z=5           \n"
        ^ ")                                    \n"
        );

        (0, "X", "g", RealBound.(of_constant (OurFloat.of_int 5)),
          "(GOAL EXPECTEDCOMPLEXITY)            \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))      \n"
        ^ "(VAR X)                              \n"
        ^ "(RULES                               \n"
        ^ "  f(X) -> g(X+Z) :|: Z<=5 && Z>=0    \n"
        ^ ")                                    \n"
        );

        (0, "X", "g", RealBound.(abs @@ varx + of_constant (OurFloat.of_int 5)),
          "(GOAL EXPECTEDCOMPLEXITY)            \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))      \n"
        ^ "(VAR X)                              \n"
        ^ "(RULES                               \n"
        ^ "  f(X) -> g(Z) :|: Z<=5 && Z>=0      \n"
        ^ ")                                    \n"
        );

        (0, "X", "g", RealBound.(of_constant (OurFloat.of_int 50)),
          "(GOAL EXPECTEDCOMPLEXITY)                   \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))             \n"
        ^ "(VAR X)                                     \n"
        ^ "(RULES                                      \n"
        ^ "  f(X) -> g(X+Z*Y) :|: Z<=5 && Z>=0 && Y=10 \n"
        ^ ")                                           \n"
        );

        (0, "X", "g", RealBound.(of_constant @@ OurFloat.of_int 100),
          "(GOAL EXPECTEDCOMPLEXITY)                     \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))               \n"
        ^ "(VAR X)                                       \n"
        ^ "(RULES                                        \n"
        ^ "  f(X) -> g(X+Z*Y) :|: Z<=5 && Z>=-10 && Y=10 \n"
        ^ ")                                             \n"
        );

        (0, "X", "g", RealBound.zero,
          "(GOAL EXPECTEDCOMPLEXITY)            \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))      \n"
        ^ "(VAR X)                              \n"
        ^ "(RULES                               \n"
        ^ "  f(X) -> 0.5:g(X) :+: 0.5:h(3*X)    \n"
        ^ ")                                    \n"
        );

        (0, "X", "h", RealBound.(abs varx),
          "(GOAL EXPECTEDCOMPLEXITY)            \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))      \n"
        ^ "(VAR X)                              \n"
        ^ "(RULES                               \n"
        ^ "  f(X) -> 0.5:g(X) :+: 0.5:h(3*X)    \n"
        ^ ")                                    \n"
        );

        (0, "X", "g", RealBound.(of_constant (OurFloat.of_float 1.5) * abs varx),
          "(GOAL EXPECTEDCOMPLEXITY)            \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))      \n"
        ^ "(VAR X)                              \n"
        ^ "(RULES                               \n"
        ^ "  f(X) -> 0.5:g(0) :+: 0.5:g(3*X)    \n"
        ^ ")                                    \n"
        );
      ];
    "concave_convexe_check" >::: List.map
      (
        fun (bstr,conc_conv) ->
          let bound = Readers.read_bound bstr |> RealBound.of_intbound in
          let (valid,err_string) =
            match conc_conv with
            | Concave ->
              (ExpLocalSizeBound.bound_is_concave bound, "Bound " ^ bstr ^ " is not concave as expected!")
            | Convexe ->
              (ExpLocalSizeBound.bound_is_convexe bound, "Bound " ^ bstr ^ " is not convexe as expected!")
            | None    ->
              match (ExpLocalSizeBound.bound_is_concave bound, ExpLocalSizeBound.bound_is_convexe bound) with
              | (true,true)   -> (false, "Bound " ^ bstr ^ "is convexe and concave!")
              | (true,false)  -> (false, "Bound " ^ bstr ^ "is concave!")
              | (false,true)  -> (false, "Bound " ^ bstr ^ "is convexe!")
              | (false,false) -> (true, "")

          in
          "" >:: fun _ -> assert_bool err_string valid
      )
      [
        ("|A|", Concave);
        ("|A|", Convexe);

        ("|A| + |B|", Concave);
        ("|A| + |B|", Convexe);

        ("|A| + |B| + |C|", Concave);
        ("|A| + |B| + |C|", Convexe);

        ("|A| + |B| + |C| + |D|", Concave);
        ("|A| + |B| + |C| + |D|", Convexe);

        ("|A| * |B|", None);

        ("|A| * |B| * |C|", None);

        ("max {1,|A|}", Convexe);
        ("min {1,|A|}", Concave);

        ("max {|A|,|B|}", Convexe);
        ("min {|A|,|B|}", Concave);

        ("max {1,|A|,|B|}", Convexe);
        ("min {1,|A|,|B|}", Concave);

        ("max {1,|A|,|B|, |A|*|B|}", None);
        ("min {1,|A|,|B|, |A|*|B|}", None);
      ]
  ]
