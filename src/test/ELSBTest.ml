open Batteries
open ProgramTypes
open Formulas
open BoundsInst
open OUnit2

module Solver = SMT.Z3Solver

let bounds b upper_b = 
  let bound_diff = RealBound.(upper_b - b) in
  try
    Solver.bound_lt_zero (RealFormula.mk_true) bound_diff
    |> not
  (* inf *)
  with Failure s -> 
    s = "inf not supported in SMT-Solving"

let tests = 
  let varx = RealBound.of_var @@ Var.of_string "X" in
  let vary = RealBound.of_var @@ Var.of_string "Y" in
  "ELSB" >::: 
    List.map 
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
        (0, "X", "g", RealBound.(abs varx), 
          "(GOAL EXPECTEDCOMPLEXITY)            \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))      \n"
        ^ "(VAR X)                              \n"
        ^ "(RULES                               \n"
        ^ "  f(X) -> g(X)                       \n"
        ^ ")                                    \n"
        );

        (0, "X", "g", RealBound.(abs varx), 
          "(GOAL EXPECTEDCOMPLEXITY)            \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))      \n"
        ^ "(VAR X Y)                            \n"
        ^ "(RULES                               \n"
        ^ "  f(X,Y) -> g(X,2*Y)                 \n"
        ^ ")                                    \n"
        );

        (0, "Y", "g", RealBound.(abs @@ (of_constant @@ OurFloat.of_int 2) * vary), 
          "(GOAL EXPECTEDCOMPLEXITY)            \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))      \n"
        ^ "(VAR X Y)                            \n"
        ^ "(RULES                               \n"
        ^ "  f(X,Y) -> g(X,2*Y)                 \n"
        ^ ")                                    \n"
        );

        (0, "Y", "g", RealBound.(abs @@ (of_constant @@ OurFloat.of_int 2) * vary - (of_constant @@ OurFloat.of_int 5) * varx), 
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

        (0, "X", "g", RealBound.(varx + of_constant (OurFloat.of_int 5)), 
          "(GOAL EXPECTEDCOMPLEXITY)            \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))      \n"
        ^ "(VAR X)                              \n"
        ^ "(RULES                               \n"
        ^ "  f(X) -> g(X+Z) :|: Z=5             \n"
        ^ ")                                    \n"
        );

        (0, "X", "g", RealBound.(varx + of_constant (OurFloat.of_int 10)), 
          "(GOAL EXPECTEDCOMPLEXITY)            \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))      \n"
        ^ "(VAR X)                              \n"
        ^ "(RULES                               \n"
        ^ "  f(X) -> g(X+2*Z) :|: Z=5             \n"
        ^ ")                                    \n"
        );

        (0, "X", "g", RealBound.(varx + of_constant (OurFloat.of_int 25)), 
          "(GOAL EXPECTEDCOMPLEXITY)            \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))      \n"
        ^ "(VAR X)                              \n"
        ^ "(RULES                               \n"
        ^ "  f(X) -> g(X+Z*Z) :|: Z=5           \n"
        ^ ")                                    \n"
        );

        (0, "X", "g", RealBound.(varx + of_constant (OurFloat.of_int 5)), 
          "(GOAL EXPECTEDCOMPLEXITY)            \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))      \n"
        ^ "(VAR X)                              \n"
        ^ "(RULES                               \n"
        ^ "  f(X) -> g(X+Z) :|: Z<=5 && Z>=0    \n"
        ^ ")                                    \n"
        );

        (0, "X", "g", RealBound.(varx + of_constant (OurFloat.of_int 50)), 
          "(GOAL EXPECTEDCOMPLEXITY)                   \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))             \n"
        ^ "(VAR X)                                     \n"
        ^ "(RULES                                      \n"
        ^ "  f(X) -> g(X+Z*Y) :|: Z<=5 && Z>=0 && Y=10 \n"
        ^ ")                                           \n"
        );

        (0, "X", "g", RealBound.(abs @@ varx + max 
                                  ((of_constant @@ OurFloat.of_int 5) * (of_constant @@ OurFloat.of_int 10)) 
                                  ((of_constant @@ OurFloat.of_int (-10)) * (of_constant @@ OurFloat.of_int 10))), 
          "(GOAL EXPECTEDCOMPLEXITY)                     \n"
        ^ "(STARTTERM (FUNCTIONSYMBOLS f))               \n"
        ^ "(VAR X)                                       \n"
        ^ "(RULES                                        \n"
        ^ "  f(X) -> g(X+Z*Y) :|: Z<=5 && Z>=-10 && Y=10 \n"
        ^ ")                                             \n"
        );
      ]