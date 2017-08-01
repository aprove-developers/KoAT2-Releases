open Batteries
open OUnit2

module Reader = Readers.MakeReader(Mocks.TransitionGraph)

let assert_equal_string =
  assert_equal ~cmp:String.equal

let suite =
  "Graphs" >::: (
    List.map (fun (testname, vars, transitions) ->
        testname >:: (fun _ ->
                let varstring = String.concat "" ["( VAR "; vars; " )"]
                and rulestring = String.concat "" ["( RULES\n"; String.concat "\n" transitions; "\n)"] in
                let text = String.concat "\n" [varstring; rulestring] in
                Printf.fprintf stderr "%s\n" text;
                assert_equal_string (Mocks.TransitionGraph.to_string (Reader.read_transitiongraph text)) text))
             [
               (*("B", "A", [" a ( A ) -> t ( a ( A ) ) :|: A > 0 "]);*)
             ]
  )                  
