open Batteries
open OUnit2

module Reader = Readers.Make(Mocks.TransitionGraph)

let assert_equal_string =
  assert_equal ~cmp:String.equal ~printer:(fun x -> x)

let suite =
  "Graphs" >::: (
    List.map (fun (testname, vars, transitions) ->
        testname >:: (fun _ ->
                let varstring = String.concat " " ["("; "VAR"; vars; ")"]
                and rulestring = String.concat " " ["("; "RULES"; "\n"; String.concat "\n" transitions; ")"] in
                let text = varstring ^ "\n" ^ rulestring in
                assert_equal_string (Mocks.TransitionGraph.to_string (Reader.read_transitiongraph text)) text))
             [
               ("No var, no guard, self loop", "", ["a (  ) -> t ( a (  ) )"]);
               ("No var, trivial guard, self loop", "", ["a (  ) -> t ( a (  ) ) :|: 1 > 0"]);
               ("No var, two locs", "", ["a (  ) -> t ( b (  ) )"]);
               ("No var, two locs", "", ["a (  ) -> t ( b (  ) )"]);
               ("One var, self loop", "A", ["a ( A ) -> t ( b ( A ) )"]);
             ]
  )                  
