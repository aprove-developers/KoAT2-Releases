open Batteries
open OUnit2
open Helper
open Program.Types
open Formulas
   
let tests = 
  "Invariant Generation" >::: [
      
      ("Simple" >:::
         List.map (fun (l, l', invariant, program_str) ->
             program_str >:: (fun _ ->
                     let result_program =
                       program_str
                       |> Readers.read_program_simple
                       |> InvariantGeneration.transform_program
                       |> MaybeChanged.unpack
                     in
                     let fulfiled =
                       result_program
                       |> Program.graph
                       |> (fun graph -> TransitionGraph.find_all_edges graph (Location.of_string l) (Location.of_string l'))
                       |> List.exists (fun (l,t,l') -> SMT.Z3Solver.tautology (Formula.Infix.(Formula.mk (TransitionLabel.guard t) => Readers.read_formula invariant)))
                     in
                     reset ();
                     assert_bool (String.concat " " [invariant; "was not generated for a transition from location"; l; "to"; l'; "in a result program"; Program.to_simple_string result_program]) fulfiled))
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
                  ]
      )
    ]
