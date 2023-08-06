open Koat2
open Batteries
open Bounds
open OUnit2
open Helper
open ProgramModules

let preprocess =
  Preprocessor.(
    StandardProgram.process
      process_till_fixpoint
      [InvariantGeneration; CutUnsatisfiableTransitions; CutUnreachableLocations]
  )

let conf = Analysis.default_configuration
module Analysis = Analysis.Make(ProgramModules)

(** Returns an overall timebound for the given program.*)
let find_timebound ?(mprf_max_depth = 1) (program: Program.t): Bound.t =
  (program, Approximation.empty)
  |> Tuple2.map1 preprocess
  |> (fun (program, appr) ->
    Analysis.improve ~preprocess ~conf:({conf with run_mprf_depth = Some mprf_max_depth}) program appr
    |> fun (program,appr) -> Approximation.program_timebound appr program
  )

(** Returns an overall costbound for the given program. *)
let find_costbound (program: Program.t): Bound.t =
  (program, Approximation.empty)
  |> Tuple2.map1 preprocess
  |> (fun (program, appr) ->
    Analysis.improve ~preprocess ~conf program appr
    |> fun (program,appr) -> Approximation.program_costbound appr program
  )

(** Shouldnt be here *)
module M = Map.Make(Var)
let from entries =
  let addEntry entry = match entry with
    | (key, value) -> M.add key value in
  List.fold_left (fun map keyadder -> keyadder map) M.empty (List.map addEntry entries)

let smaller_or_equal (vars: VarSet.t) b1 b2 =
  [-999999;0;999999]
  |> Enum.repeat ~times:(Base.Set.length vars)
  |> List.of_enum
  |> List.n_cartesian_product
  |> List.for_all (fun assignment ->
         let valuation =
           assignment
           |> List.map Bound.of_int
           |> List.combine (Base.Set.to_list vars)
           |> from
           |> (fun map var -> M.find var map)
         in
         (
           Bound.(substitute_f valuation b1 <= substitute_f valuation b2) |? false
         )
       )

let tests =
  "Overall costbound" >::: [

      ("Simple" >:::
         List.map (fun (minimal_sound_costbound_str, wanted_costbound_str, program_str) ->
             program_str >:: (fun _ ->
                     let program = Readers.read_program_simple program_str in
                     let minimal_sound_costbound = Readers.read_bound minimal_sound_costbound_str in
                     let wanted_costbound = Option.map Readers.read_bound wanted_costbound_str |? minimal_sound_costbound in
                     let costbound = find_costbound program in
                     assert_bool (String.concat " " [Bound.to_string costbound; "is not sound, since it is smaller than"; Bound.to_string minimal_sound_costbound])
                                 (smaller_or_equal (Program.vars program) minimal_sound_costbound costbound);
                     assert_bool (String.concat " " [Bound.to_string costbound; "is not as small as wanted, since it is greater than"; Bound.to_string wanted_costbound])
                                 (smaller_or_equal (Program.vars program) costbound wanted_costbound)))
                  [
                    (* Constant bound *)

                    ("1", None,
                     "a -> b(x)");

                    ("2", None,
                     "a -> b(x), b -> c(x)");

                    ("2", Some "4",
                     "a -> b(x), b -> c(x), b -> d(x)");

                    ("3", Some "5",
                     "a -> b(x), b -> c(x), b -> d(x), c -> d(x)");

                    ("11", Some "12",
                     "a -> b(10), b -> b(x-1) :|: x>0");

                    (* TODO Possible? ("6", None, "a -> b(10), b -> b(x-2) :|: x>0"); *)

                    (* Linear bound *)

                    ("Arg_0+1", None,
                     "a -> b(x), b -> b(x-1) :|: x>0");

                    ("Arg_0+Arg_1+1", None,
                     "a -> b(x,y), b -> b(x-1,y) :|: x+y>0");

                    ("Arg_0+Arg_1+1", None,
                     "a -> b(x,y), b -> b(x-1,y) :|: x>y");

                    ("Arg_0+Arg_1+1", None,
                     "a -> b(x,y), b -> b(x+1,y) :|: x<y");

                    ("Arg_1+1", None,
                     "a -> b(0,y), b -> b(x+1,y) :|: x<y");

                    (* This is not the smallest possible bound. But it is the smallest that can be computed using monotone bounds. *)
                    ("Arg_0+Arg_1+2 + Arg_1", None,
                     "a -> b(x,y), b -> b(x-1,y) :|: x>0, b -> c(x,y), c -> c(x,y-1) :|: y>0");

                    (* This is not the smallest possible bound. But it is the smallest that can be computed using monotone bounds. *)
                    ("Arg_0+(Arg_1+Arg_0)+2 + Arg_1", None,
                     "a -> b(x,y), b -> b(x-1,y+1) :|: x>0, b -> c(x,y) :|: x<=0, c -> c(x,y-1) :|: y>0");

                    (* Quadratic bound *)
                  ]
      );

      ("Correct complexity class" >:::
         let open Bound in
         List.map (fun (expected_complexity, program_str) ->
             program_str >:: (fun _ ->
                     let complexity = (asymptotic_complexity % find_costbound % Readers.read_program_simple) program_str in
                     assert_equal ~cmp:equal_complexity ~printer:show_complexity expected_complexity complexity))
                  [
                    (Inf, "a -> b(), b -> b()");
                    (Inf, "a -> b(x), b -> b(x-1) :|: x>0, b -> b(x+1) :|: x<=0");
                    (Polynomial 0, "a -> b(), b -> c()");
                    (Polynomial 0, "a -> b(), b -> c(), a -> c()");
                    (* TODO Problem with constant ranking functions (Polynomial 0, "a -> b(x), b -> b(x-x) :|: x>0"); *)
                    (Polynomial 0, "a -> b(x), b -> b(x-1) :|: x>x");
                    (Polynomial 1, "a -> b(x), b -> b(x-1) :|: x>0");
                    (Polynomial 1, "a -> b(x,y), b -> b(x-1,y) :|: x>y");
                    (Polynomial 1, "a -> b(x,y), b -> b(x-1,y) :|: x>0, b -> c(x,y), c -> c(x+1,y) :|: x<y");
                    (Polynomial 1, "a -> b(x,y), b -> b(x+1,y-1) :|: y>0, b -> c(x,y), c -> c(x-1,y) :|: x > 0");
                    (* Non-linear not supported by Z3 (Polynomial 2, "a -> b(x), b -> b(x-1) :|: x^2>0"); *)
                    (Polynomial 2, "a -> b(x,y), b -> b(x+y,y-1) :|: y>0, b -> c(x,y), c -> c(x-1,y) :|: x > 0");
                    (Exponential 1, "a -> b(x,y), b -> b(2*x,y-1) :|: y>0, b -> c(x,y), c -> c(x-1,y) :|: x > 0");
                    (Exponential 1, "a -> b(x,y,z), b -> c(x+y,y,z-1) :|: z>0, c -> b(x,x,z) :|: z>0, c -> d(x,y,z), d -> d(x-1,y,z) :|: x>0");
                  ]
      );

    ]
