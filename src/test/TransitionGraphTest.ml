open Batteries
open OUnit2
open Helper

module Reader = Readers

let assert_equal_string =
  assert_equal ~cmp:String.equal ~printer:(fun x -> x)

module Program = Program

module TransitionSet = Set.Make(Program.Transition)
               
let suite =
  "Graphs" >::: [
      (
        let test_folder folder =
          ("examples/" ^ folder ^ "/") >::: (
            let files = Array.filter (fun s -> String.ends_with s ".koat") (Sys.readdir ("../../examples/" ^ folder))
            and test (file : string): unit = try ignore (Reader.read_file ("../../examples/" ^ folder ^ "/" ^ file)) with
                                             | Reader.Error msg -> failwith msg
                                             | TransitionLabel.RecursionNotSupported -> skip_if true "Recursion not supported" in
            Array.to_list (Array.map (fun s -> (s >:: (fun _ -> test s))) files)) in
        "Examples" >::: List.map test_folder ["KoAT-2013"; "KoAT-2014"; "SAS10"; "T2"]
      );
      (
        "pre(t)" >:: (fun _ ->
          let program = Reader.read_file "../../examples/KoAT-2013/sect1-lin.koat" in
          let transition = Program.TransitionGraph.find_edge (Program.graph program) (Program.Location.of_string "l1") (Program.Location.of_string "l2") in
          assert_equal_int 2 (TransitionSet.cardinal (Program.pre program transition))
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
                       let program = Reader.read_file "../../examples/KoAT-2013/sect1-lin.koat" in
                       let (l,t,l') = Program.TransitionGraph.find_edge (Program.graph program) (Program.Location.of_string l) (Program.Location.of_string l') in
                       TransitionLabel.(assert_equal_bound
                                                  (Bound.of_poly (Reader.read_polynomial bound))
                                                  LocalSizeBound.(as_bound (sizebound_local Upper t (Var.of_string var)))
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
