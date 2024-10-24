open Koat2
open Batteries
open Bounds
open OUnit2
open ProgramModules

let preprocess =
  Preprocessor.(
    StandardProgram.process process_till_fixpoint
      [ InvariantGeneration; CutUnsatisfiableTransitions; CutUnreachableLocations ])


let default_conf = Analysis.default_configuration

let mprf5_conf =
  Analysis.
    {
      default_configuration with
      local_configuration = { default_local_configuration with run_mprf_depth = Some 5 };
    }


let twn_conf =
  Analysis.
    {
      default_configuration with
      local_configuration = { default_local_configuration with twn = true; twnlog = true };
    }


let twn_size_conf =
  Analysis.
    {
      default_configuration with
      local_configuration =
        {
          default_local_configuration with
          twn = true;
          twnlog = true;
          closed_form_size_bounds = ComputeClosedFormSizeBounds;
        };
    }


let unsolvable_conf =
  Analysis.
    {
      default_configuration with
      local_configuration = { default_local_configuration with twn = true; twnlog = true; unsolvable = true };
    }


module Analysis = Analysis.Classical (Bounds.Bound)

(** Returns an overall costbound for the given program. *)
let find_costbound ?(conf = default_conf) (program : Program.t) : Bound.t =
  (program, Approximation.empty) |> Tuple2.map1 preprocess |> fun (program, appr) ->
  Analysis.improve ~preprocess ~conf program appr |> fun (program, appr) ->
  Approximation.program_costbound appr program


module M = Map.Make (Var)
(** Shouldnt be here *)

let from entries =
  let addEntry entry =
    match entry with
    | key, value -> M.add key value
  in
  List.fold_left (fun map keyadder -> keyadder map) M.empty (List.map addEntry entries)


let smaller_or_equal (vars : VarSet.t) b1 b2 =
  [ -999999; 0; 999999 ]
  |> Enum.repeat ~times:(Base.Set.length vars)
  |> List.of_enum |> List.n_cartesian_product
  |> List.for_all (fun assignment ->
         let valuation =
           assignment |> List.map Bound.of_int |> List.combine (Base.Set.to_list vars) |> from
           |> fun map var -> M.find var map
         in
         Bound.(substitute_f valuation b1 <= substitute_f valuation b2) |? false)


let tests =
  "Overall costbound"
  >::: [
         "Simple"
         >::: List.map
                (fun (minimal_sound_costbound_str, wanted_costbound_str, program_str) ->
                  program_str >:: fun _ ->
                  let program = Readers.read_program_simple program_str in
                  let minimal_sound_costbound = Readers.read_bound minimal_sound_costbound_str in
                  let wanted_costbound =
                    Option.map Readers.read_bound wanted_costbound_str |? minimal_sound_costbound
                  in
                  let costbound = find_costbound program in
                  assert_bool
                    (String.concat " "
                       [
                         Bound.to_string costbound;
                         "is not sound, since it is smaller than";
                         Bound.to_string minimal_sound_costbound;
                       ])
                    (smaller_or_equal (Program.vars program) minimal_sound_costbound costbound);
                  assert_bool
                    (String.concat " "
                       [
                         Bound.to_string costbound;
                         "is not as small as wanted, since it is greater than";
                         Bound.to_string wanted_costbound;
                       ])
                    (smaller_or_equal (Program.vars program) costbound wanted_costbound))
                [
                  (* Constant bound *)
                  ("1", None, "a(x) -> b(x)");
                  ("2", None, "a(x) -> b(x), b(x) -> c(x)");
                  ("2", Some "4", "a(x) -> b(x), b(x) -> c(x), b(x) -> d(x)");
                  ("3", Some "5", "a(x) -> b(x), b(x) -> c(x), b(x) -> d(x), c(x) -> d(x)");
                  ("11", Some "12", "a(x) -> b(10), b(x) -> b(x-1) :|: x>0");
                  (* TODO Possible? ("6", None, "a -> b(10), b -> b(x-2) :|: x>0"); *)

                  (* Linear bound *)
                  ("Arg_0+1", None, "a(x) -> b(x), b(x) -> b(x-1) :|: x>0");
                  ("Arg_0+Arg_1+1", None, "a(x,y) -> b(x,y), b(x,y) -> b(x-1,y) :|: x+y>0");
                  ("Arg_0+Arg_1+1", None, "a(x,y) -> b(x,y), b(x,y) -> b(x-1,y) :|: x>y");
                  ("Arg_0+Arg_1+1", None, "a(x,y) -> b(x,y), b(x,y) -> b(x+1,y) :|: x<y");
                  ("Arg_1+1", None, "a(x,y) -> b(0,y), b(x,y) -> b(x+1,y) :|: x<y");
                  (* This is not the smallest possible bound. But it is the smallest that can be computed using monotone bounds. *)
                  ( "Arg_0+Arg_1+2 + Arg_1",
                    None,
                    "a(x,y) -> b(x,y), b(x,y) -> b(x-1,y) :|: x>0, b(x,y) -> c(x,y), c(x,y) -> c(x,y-1) :|: \
                     y>0" );
                  (* This is not the smallest possible bound. But it is the smallest that can be computed using monotone bounds. *)
                  ( "Arg_0+(Arg_1+Arg_0)+2 + Arg_1",
                    None,
                    "a(x,y) -> b(x,y), b(x,y) -> b(x-1,y+1) :|: x>0, b(x,y) -> c(x,y) :|: x<=0, c(x,y) -> \
                     c(x,y-1) :|: y>0" );
                  (* Quadratic bound *)
                ];
         ("Correct complexity class"
         >:::
         let open Bound in
         List.map
           (fun (expected_complexity, program_str, conf) ->
             program_str >:: fun _ ->
             let complexity =
               (asymptotic_complexity % find_costbound ~conf % Readers.read_program_simple) program_str
             in
             let error_msg =
               "Asymptotic Complexity " ^ Bound.show_complexity complexity
               ^ " does not match expected complexity "
               ^ Bound.show_complexity expected_complexity
             in
             assert_bool error_msg @@ Bound.equal_complexity expected_complexity complexity)
           [
             (Inf, "a() -> b(), b() -> b()", default_conf);
             (Inf, "a(x) -> b(x), b(x) -> b(x-1) :|: x>0, b(x) -> b(x+1) :|: x<=0", default_conf);
             (LogarithmicPolynomial (0, 0), "a() -> b(), b() -> c()", default_conf);
             (LogarithmicPolynomial (0, 0), "a() -> b(), b() -> c(), a() -> c()", default_conf);
             (* TODO Problem with constant ranking functions (LogarithmicPolynomial (0,0), "a -> b(x), b -> b(x-x) :|: x>0", default_conf); *)
             (LogarithmicPolynomial (0, 0), "a(x) -> b(x), b(x) -> b(x-1) :|: x>x", default_conf);
             (LogarithmicPolynomial (0, 1), "a(x) -> b(x), b(x) -> b(x-1) :|: x>0", default_conf);
             (LogarithmicPolynomial (0, 1), "a(x,y) -> b(x,y), b(x,y) -> b(x-1,y) :|: x>y", default_conf);
             ( LogarithmicPolynomial (0, 1),
               "a(x,y) -> b(x,y), b(x,y) -> b(x-1,y) :|: x>0, b(x,y) -> c(x,y), c(x,y) -> c(x+1,y) :|: x<y",
               default_conf );
             ( LogarithmicPolynomial (0, 1),
               "a(x,y) -> b(x,y), b(x,y) -> b(x+1,y-1) :|: y>0, b(x,y) -> c(x,y), c(x,y) -> c(x-1,y) :|: x > \
                0",
               default_conf );
             (* Non-linear not supported by Z3 (LogarithmicPolynomial (0,2), "a -> b(x), b -> b(x-1) :|: x^2>0", default_conf); *)
             ( LogarithmicPolynomial (0, 2),
               "a(x,y) -> b(x,y), b(x,y) -> b(x+y,y-1) :|: y>0, b(x,y) -> c(x,y), c(x,y) -> c(x-1,y) :|: x > \
                0",
               default_conf );
             ( Exponential 1,
               "a(x,y) -> b(x,y), b(x,y) -> b(2*x,y-1) :|: y>0, b(x,y) -> c(x,y), c(x,y) -> c(x-1,y) :|: x > \
                0",
               default_conf );
             ( Exponential 1,
               "a(x,y,z) -> b(x,y,z), b(x,y,z) -> c(x+y,y,z-1) :|: z>0, c(x,y,z) -> b(x,x,z) :|: z>0, \
                c(x,y,z) -> d(x,y,z), d(x,y,z) -> d(x-1,y,z) :|: x>0",
               default_conf );
             (* MPRF *)
             (Inf, "a(x,y) -> b(x,y), b(x,y) -> b(x+y,y-1) :|: x > 0", default_conf);
             (LogarithmicPolynomial (0, 1), "a(x,y) -> b(x,y), b(x,y) -> b(x+y,y-1) :|: x > 0", mprf5_conf);
             ( LogarithmicPolynomial (0, 1),
               "a(x,y,z) -> b(x,y,z), b(x,y,z) -> b(x+y,y+z,z-1) :|: x > 0",
               mprf5_conf );
             ( LogarithmicPolynomial (0, 1),
               "a(x,y,z,u) -> b(x,y,z,u), b(x,y,z,u) -> b(x+y,y+z,z+u,u-1) :|: x > 0",
               mprf5_conf );
             ( LogarithmicPolynomial (0, 1),
               "a(x,y,z,u,v) -> b(x,y,z,u,v), b(x,y,z,u,v) -> b(x+y,y+z,z+u,u+v,v-1) :|: x > 0",
               mprf5_conf );
             (* This would require depth > 5 *)
             ( Inf,
               "a(x,y,z,u,v,p) -> b(x,y,z,u,v,p), b(x,y,z,u,v,p) -> b(x+y,y+z,z+u,u+v,v+p,p-1) :|: x > 0",
               mprf5_conf );
             (* TWN based on twn001 *)
             (Inf, "a(x,y) -> b(x,y), b(x,y) -> b(2*x, 3*y) :|: x >= y && y >= 1", default_conf);
             ( LogarithmicPolynomial (1, 0),
               "a(x,y) -> b(x,y), b(x,y) -> b(2*x, 3*y) :|: x >= y && y >= 1",
               twn_conf );
             ( Inf,
               "a(a,b,c,d) -> b(a,b,c,d), b(a,b,c,d) -> b(a,-2*b,c + c^2 + a^2,-4*c + 2*c^2 + 3*d + a^2) :|: \
                b != 0 && b^2 - a^5 < 2*c - d",
               twn_conf );
             ( LogarithmicPolynomial (1, 0),
               "a(a,b,c,d) -> b(a,b,c,d), b(a,b,c,d) -> b(a,-2*b,c + c^2 + a^2,-4*c + 2*c^2 + 3*d + a^2) :|: \
                b != 0 && b^2 - a^5 < 2*c - d",
               unsolvable_conf );
             ( LogarithmicPolynomial (0, 6),
               "a(a,b,c) -> b(a,b,c) :|: c < 0,b(a,b,c) ->b(a - b^2,b + c^2,c) :|: a > 0,b(a,b,c) -> \
                c(a,b,c) :|: a <= 0,c(a,b,c) -> c(a,b - 1,c) :|: b > 0",
               twn_size_conf );
             ( LogarithmicPolynomial (1, 0),
               "a(x,y,z) -> b(1,y,z), b(x,y,z) -> b(2*x, 3*y,z) :|: x >= y && y >= 1, b(x,y,z) -> c(1,z,z), \
                c(x,y,z) -> c(2*x, 3*y, z) :|: x >= y && y >= 1",
               twn_conf );
             ( LogarithmicPolynomial (1, 1),
               "a(x,y,z,u,v) -> b(x,y,z,u,v), b(x,y,z,u,v) -> c(u,v,z-1,u,v) :|: z > 0, c(x,y,z,u,v) -> \
                c(2*x,3*y,z,u,v) :|: x >= y && y >= 1, c(x,y,z,u,v) -> b(x,y,z,u,v)",
               twn_conf );
             ( LogarithmicPolynomial (1, 1),
               "a(x,y,z) -> b(x,y,z), b(x,y,z) -> c(z,1,z-1) :|: z > 0, c(x,y,z) -> c(2*x,3*y,z) :|: x >= y \
                && y >= 1, c(x,y,z) -> d(z,1,z), d(x,y,z) -> d(2*x,3*y,z) :|: x>= y && y >= 1, d(x,y,z) -> \
                b(x,y,z)",
               twn_conf );
             ( LogarithmicPolynomial (1, 2),
               "a(x,y,z,u,v) -> b(x,y,z,u,v), b(x,y,z,u,v) -> c(x,y,z-1,z,v) :|: z > 0, c(x,y,z,u,v) -> \
                d(u,v,z,u-1,v) :|: u > 0, d(x,y,z,u,v) -> d(2 * x, 3 * y,z,u,v) :|: x >= y && y >= 1, \
                d(x,y,z,u,v) -> c(y,y,z,u,v), d(x,y,z,u,v) -> b(x,y,z,u,v)",
               twn_conf );
           ]);
       ]
