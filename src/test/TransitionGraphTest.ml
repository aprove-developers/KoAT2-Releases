open Batteries
open OUnit2
open Helper
open ProgramTypes
open RVGTypes
   
let suite =
  "Graphs" >::: [
      (
        let test_folder folder =
          ("examples_old_input/" ^ folder ^ "/") >::: (
            let files = Array.filter (fun s -> String.ends_with s ".koat") (Sys.readdir ("../../examples/" ^ folder))
            and test (file : string): unit = try ignore (Readers.read_file ("../../examples/" ^ folder ^ "/" ^ file)) with
                                             | ParserUtil.Error msg -> failwith msg
                                             | TransitionLabel.RecursionNotSupported -> skip_if true "Recursion not supported" in
            Array.to_list (Array.map (fun s -> (s >:: (fun _ -> test s))) files)) in
        "Examples" >::: List.map test_folder ["KoAT-2013"; "KoAT-2014"; "SAS10"; "T2"]
      );
      (
        let test_folder folder =
          ("examples_new_input/" ^ folder ^ "/") >::: (
            let files = Array.filter (fun s -> String.ends_with s ".koat") (Sys.readdir ("../../examples/" ^ folder))
            and test (file : string): unit = try ignore (Readers.read_file ("../../examples/" ^ folder ^ "/" ^ file)) with
                                             | ParserUtil.Error msg -> failwith msg
                                             | TransitionLabel.RecursionNotSupported -> skip_if true "Recursion not supported" in
            Array.to_list (Array.map (fun s -> (s >:: (fun _ -> test s))) files)) in
        "Examples" >::: List.map test_folder ["CageKoAT-Input-Examples/weightedExamples/badExamples" ;"CageKoAT-Input-Examples/weightedExamples/constantWeights"; "CageKoAT-Input-Examples/weightedExamples/simplePolyWeights"; "CageKoAT-Input-Examples/debugExamples"; "CageKoAT-Input-Examples/cexamples"]
      );
      (
        "pre(t)" >:: (fun _ ->
          let program = Readers.read_file "../../examples/KoAT-2013/sect1-lin.koat" in
          let transition = TransitionGraph.find_edge (Program.graph program) (Location.of_string "l1") (Location.of_string "l2") in
          assert_equal_int 2 (Enum.count (Program.pre program transition))
        )
      );
      (
        "sizebound_local" >::: (
          [
            ("l1", "l2", "A", "0");
            (* TODO We have to redefine good bounds here: ("l1", "l1", "A", "A-1"); *)
          ]
        |> List.map (fun (l,l',var,bound) ->
               "Bound from " ^ l ^ " to " ^ l' ^ " for var " ^ var ^ " is " ^ bound ^ "?" >:: (fun _ ->
                       let program = Readers.read_file "../../examples/KoAT-2013/sect1-lin.koat" in
                       let t = TransitionGraph.find_edge (Program.graph program) (Location.of_string l) (Location.of_string l') in
                       TransitionLabel.(assert_equal_bound
                                                  (Bound.of_poly (Readers.read_polynomial bound))
                                                  LocalSizeBound.(sizebound_local program `Upper t (Var.of_string var) |> Option.map as_bound |? default `Upper)
                       )
                     )
             )
        )
                
      );
      (
        "Print" >:: (fun _ ->
          GraphPrint.print_system ~label:TransitionLabel.to_string ~outdir:(Fpath.v "output") ~file:"sect1-lin" (Readers.read_file "../../examples/KoAT-2013/sect1-lin.koat");
          "../../examples/KoAT-2013/sect1-lin.koat"
          |> Readers.read_file
          |> tap (fun program -> GraphPrint.print_rvg `Lower ~label:RV.to_id_string ~outdir:(Fpath.v "output") ~file:"sect1-lin" program)
          |> tap (fun program -> GraphPrint.print_rvg `Upper ~label:RV.to_id_string ~outdir:(Fpath.v "output") ~file:"sect1-lin" program)
          |> ignore
        )
      );
    ]
