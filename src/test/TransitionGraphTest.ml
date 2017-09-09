open Batteries
open OUnit2

module Reader = Readers.Make(ProgramImpl.StdProgram)

let assert_equal_string =
  assert_equal ~cmp:String.equal ~printer:(fun x -> x)

let suite =
  "Graphs" >::: [
      (
        let test_folder folder =
          ("examples/" ^ folder ^ "/") >::: (
            let files = Array.filter (fun s -> String.ends_with s ".koat") (Sys.readdir ("../../examples/" ^ folder))
            and test (file : string): unit = try ignore (Reader.read_file ("../../examples/" ^ folder ^ "/" ^ file)) with
                                             | Reader.Error msg -> failwith msg
                                             | ProgramImpl.StdProgram.Transition.RecursionNotSupported -> skip_if true "Recursion not supported" in
            Array.to_list (Array.map (fun s -> (s >:: (fun _ -> test s))) files)) in
        "Examples" >::: List.map test_folder ["KoAT-2013"; "KoAT-2014"; "SAS10"; "T2"]
      );
    ]                  
