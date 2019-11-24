open Batteries
open OUnit2
open Helper
open ProgramTypes
open Formulas

let tests =
  "Invariant Generation" >::: [

      ("Simple" >:::
         List.map (fun (l, l', invariant, program_str) ->
             program_str >:: (fun _ ->
                     let cache = CacheManager.new_cache () in
                     let result_program =
                       program_str
                       |> Readers.read_program_simple (CacheManager.trans_id_counter cache)
                       |> InvariantGeneration.transform_program
                       |> MaybeChanged.unpack
                     in
                     let fulfiled =
                       result_program
                       |> Program.graph
                       |> (fun graph -> TransitionGraph.find_all_edges graph (Location.of_string l) (Location.of_string l'))
                       |> List.exists (fun (l,t,l') -> SMT.Z3Opt.tautology (Formula.Infix.(Formula.mk (TransitionLabel.guard t) => Readers.read_formula invariant)))
                     in
                     assert_bool (String.concat " " [invariant; "was not generated for a transition from location"; l; "to"; l'; "in a result program"; Program.to_simple_string result_program; "program_str"; program_str]) fulfiled))
                  [
                    (* Simple propagation to next transition *)
                    ("b", "c", "x > 0",
                     "a -> b(x) :|: x > 0, b -> c(x)");
                    (* More complex propagation to next transition *)
                    ("b", "c", "x > 0 && x < y",
                     "a -> b(x,y) :|: x > 0 && x < y, b -> c(x,y)");
                    (* Propagation into a constant loop *)
                    ("b", "b", "x > 0",
                     "a -> b(x) :|: x > 0, b -> b(x)");
                    (* Propagation after a constant loop *)
                    ("b", "c", "x > 0",
                     "a -> b(x) :|: x > 0, b -> b(x), b -> c(x)");
                    (* Propagation after a modifying loop *)
                    ("b", "c", "x > 0",
                     "a -> b(x) :|: x > 0, b -> b(x+1), b -> c(x)");
                    (* Fixed value propagation *)
                    ("b","b","y = 1"," a->b(x,1), b->b(x-y,y) :|: x>0");
                    (* Complex program invariants from exp2.koat *)
                    ("g","g","z = 1","f-> g(x, 1, 1),  g-> g(x - 1, 2 * y, z):|: x > 0,  g-> h(x, y, z):|: x <= 0,  h-> h(x, y - 1, 2 * z):|: y > 0,  h-> i(x, y, z):|: y <= 0, i-> i(x, y, z - 1):|: z > 0");
                    ("h","h","x<=0","f-> g(x, 1, 1),  g-> g(x - 1, 2 * y, z):|: x > 0,  g-> h(x, y, z):|: x <= 0,  h-> h(x, y - 1, 2 * z):|: y > 0,  h-> i(x, y, z):|: y <= 0, i-> i(x, y, z - 1):|: z > 0");
                    ("i","i","x<=0 && y<=0","f-> g(x, 1, 1),  g-> g(x - 1, 2 * y, z):|: x > 0,  g-> h(x, y, z):|: x <= 0,  h-> h(x, y - 1, 2 * z):|: y > 0,  h-> i(x, y, z):|: y <= 0, i-> i(x, y, z - 1):|: z > 0");
                    (* Complex program invariants from paperexample.koat *)
                    ("g","h","x>0","f-> g(1,1),g -> h(x,y) :|: y=1,h -> g(2*x,y),h -> g(x,0),g -> i(x,y) :|: y=0,i -> i(x-1,y) :|: x>=0");
                    ("g","i","x>0","f-> g(1,1),g -> h(x,y) :|: y=1,h -> g(2*x,y),h -> g(x,0),g -> i(x,y) :|: y=0,i -> i(x-1,y) :|: x>=0");
                    ("i","i","y=0","f-> g(1,1),g -> h(x,y) :|: y=1,h -> g(2*x,y),h -> g(x,0),g -> i(x,y) :|: y=0,i -> i(x-1,y) :|: x>=0");
                    ("h","g","y=1","f-> g(1,1),g -> h(x,y) :|: y=1,h -> g(2*x,y),h -> g(x,0),g -> i(x,y) :|: y=0,i -> i(x-1,y) :|: x>=0");
                    ("g","h","x+y>=1","f-> g(1,1),g -> h(x,y) :|: y=1,h -> g(2*x,y),h -> g(x,0),g -> i(x,y) :|: y=0,i -> i(x-1,y) :|: x>=0");
                    ("g","h","x+y>=2","f-> g(1,1),g -> h(x,y) :|: y=1,h -> g(2*x,y),h -> g(x,0),g -> i(x,y) :|: y=0,i -> i(x-1,y) :|: x>=0");
                    ("g","i","y=0","f-> g(1,1),g -> h(x,y) :|: y=1,h -> g(2*x,y),h -> g(x,0),g -> i(x,y) :|: y=0,i -> i(x-1,y) :|: x>=0");
                    (* TODO: This invariant is not found at the moment *)
(*                     ("g","i","y=1","f-> g(1,1),g -> h(x,y) :|: y=1,h -> g(2*x,y),h -> g(x,0),g -> i(x,y) :|: y=0,i -> i(x-1,y) :|: x>=0"); *)
                  ]
      )
    ]
