open Batteries
open OUnit2

module Reader = Readers.Make(Mocks.TransitionGraph)

let assert_equal_string =
  assert_equal ~cmp:String.equal ~printer:(fun x -> x)

let suite =
  "Graphs" >::: [
      "Simple" >::: (
        List.map (fun (testname, vars, start, transitions) ->
            testname >:: (fun _ ->
                    let goal = String.concat " " ["("; "GOAL"; "COMPLEXITY"; ")"]
                    and startterm = String.concat " " ["("; "STARTTERM"; "("; "FUNCTIONSYMBOLS"; start; ")"; ")"]
                    and varstring = String.concat " " ["("; "VAR"; vars; ")"]
                    and rulestring = String.concat " " ["("; "RULES"; "\n"; String.concat "\n" transitions; ")"] in
                    let text = String.concat "\n" [goal; startterm; varstring; rulestring] in
                    assert_equal_string (Mocks.TransitionGraph.to_string (Reader.read_transitiongraph text)) text))
                 [
                   ("No var, no guard, self loop", "", "a", ["a (  ) -> t ( a (  ) )"]);
                   ("No var, trivial guard, self loop", "", "a", ["a (  ) -> t ( a (  ) ) :|: 1 > 0"]);
                   ("No var, two locs", "", "a", ["a (  ) -> t ( b (  ) )"]);
                   ("No var, two locs", "", "a", ["a (  ) -> t ( b (  ) )"]);
                   ("One var, self loop", "A", "a", ["a ( A ) -> t ( b ( A ) )"]);
                 ]
      );
      "examples/KoAT-2013/" >::: (
        let files = Array.filter (fun s -> String.ends_with s ".koat") (Sys.readdir "../../examples/KoAT-2013")
        and test (file : string): unit = try Reader.read_file ("../../examples/KoAT-2013/" ^ file); () with
                        | Reader.Error msg -> failwith msg
                        | Mocks.TransitionGraph.Transition_.RecursionNotSupported -> skip_if true "Recursion not supported" in
        Array.to_list (Array.map (fun s -> (s >:: (fun _ -> test s; ()))) files)
      );
    ]                  
