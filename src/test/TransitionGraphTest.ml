open Batteries
open OUnit2
open Helper

module Reader = Readers.Make(ProgramImpl.StdProgram)

let assert_equal_string =
  assert_equal ~cmp:String.equal ~printer:(fun x -> x)

module Program = ProgramImpl.StdProgram

let suite =
  "Graphs" >::: [
      (
        let test_folder folder =
          ("examples/" ^ folder ^ "/") >::: (
            let files = Array.filter (fun s -> String.ends_with s ".koat") (Sys.readdir ("../../examples/" ^ folder))
            and test (file : string): unit = try ignore (Reader.read_file ("../../examples/" ^ folder ^ "/" ^ file)) with
                                             | Reader.Error msg -> failwith msg
                                             | Program.TransitionLabel.RecursionNotSupported -> skip_if true "Recursion not supported" in
            Array.to_list (Array.map (fun s -> (s >:: (fun _ -> test s))) files)) in
        "Examples" >::: List.map test_folder ["KoAT-2013"; "KoAT-2014"; "SAS10"; "T2"]
      );
      (
        "pre(t)" >:: (fun _ ->
          let program = Reader.read_file "../../examples/KoAT-2013/sect1-lin.koat" in
          let transition = Program.TransitionGraph.find_edge (Program.graph program) (Program.Location.of_string "l1") (Program.Location.of_string "l2") in
          assert_equal_int 2 (Set.cardinal (Program.pre program transition))
        )
      );
      (
        "sizebound_local" >::: (
          [
            ("l1", "l2", "A", "A");
            ("l1", "l1", "A", "A-1");
          ]
        |> List.map (fun (l,l',var,bound) ->
               "Bound from " ^ l ^ " to " ^ l' ^ " for var " ^ var ^ " is " ^ bound ^ "?" >:: (fun _ ->
                       let program = Reader.read_file "../../examples/KoAT-2013/sect1-lin.koat" in
                       let (l,t,l') = Program.TransitionGraph.find_edge (Program.graph program) (Program.Location.of_string l) (Program.Location.of_string l') in
                       Program.TransitionLabel.(assert_equal_bound
                                                  (Bound.of_poly (Reader.read_polynomial bound))
                                                  (sizebound_local Upper t (Program.Constraint_.Polynomial_.Var.of_string var))
                       )
                     )
             )
        )
                
      );
      (
        "Print" >:: (fun _ ->
          Program.print_system ~outdir:"output" ~file:"sect1-lin" (Reader.read_file "../../examples/KoAT-2013/sect1-lin.koat");
          Program.print_rvg ~outdir:"output" ~file:"sect1-lin" (Reader.read_file "../../examples/KoAT-2013/sect1-lin.koat")
        )
      );
    ]
