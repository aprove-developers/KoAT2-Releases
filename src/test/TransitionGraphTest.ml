open Batteries
open BoundsInst
open OUnit2
open Helper
open ProgramTypes
open RVGTypes

module RV = RVG.RV

let tests =
  "Graphs" >::: [
      (
        let cache = CacheManager.new_cache () in
        let trans_id_counter = CacheManager.trans_id_counter cache in
        let test_folder folder =
          ("examples_old_input/" ^ folder ^ "/") >::: (
            let files = Array.filter (fun s -> String.ends_with s ".koat") (Sys.readdir ("../../examples/" ^ folder))
            and test (file : string): unit = try ignore (Readers.read_file trans_id_counter ("../../examples/" ^ folder ^ "/" ^ file)) with
                                             | Readers.Error msg -> failwith msg
                                             | TransitionLabel.RecursionNotSupported -> skip_if true "Recursion not supported" in
            Array.to_list (Array.map (fun s -> (s >:: (fun _ -> test s))) files)) in
        "Examples" >::: List.map test_folder ["KoAT-2013"; "KoAT-2014"; "SAS10"; "T2"; "ProbabilisticExamples"]
      );
      (
        let test_folder folder =
          ("examples_new_input/" ^ folder ^ "/") >::: (
            let cache = CacheManager.new_cache () in
            let trans_id_counter = CacheManager.trans_id_counter cache in
            let files = Array.filter (fun s -> String.ends_with s ".koat") (Sys.readdir ("../../examples/" ^ folder)) in
            let test (file : string): unit =
              try
                let read_function () = ignore (Readers.read_file trans_id_counter ("../../examples/" ^ folder ^ "/" ^ file)) in
                (* Check if an exception is raised when transitions lead back to the initial location *)
                if file = "start_incoming.koat" then
                  assert_raises (Failure "Transition leading back to the initial location.") read_function
                else
                  read_function ()
              with
                | Readers.Error msg -> failwith msg
                | TransitionLabel.RecursionNotSupported -> skip_if true "Recursion not supported"
            in
            Array.to_list (Array.map (fun s -> (s >:: (fun _ -> test s))) files))
        in
        "Examples" >::: List.map test_folder ["CageKoAT-Input-Examples/weightedExamples/badExamples" ;"CageKoAT-Input-Examples/weightedExamples/constantWeights"; "CageKoAT-Input-Examples/weightedExamples/simplePolyWeights"; "CageKoAT-Input-Examples/debugExamples"; "CageKoAT-Input-Examples/cexamples"; "ProbabilisticExamples"]
      );
      (
        "pre(t)" >:: (fun _ ->
          let cache = CacheManager.new_cache () in
          let trans_id_counter = CacheManager.trans_id_counter cache in
          let program = Readers.read_file trans_id_counter "../../examples/KoAT-2013/sect1-lin.koat" in
          let transition =
            TransitionGraph.transitions (Program.graph program)
            |> TransitionSet.filter (fun (l,_,l') -> Location.(name l = "l1" && name l' = "l2"))
            |> TransitionSet.any
          in
          assert_equal_int 2 (List.length (Program.pre (CacheManager.pre_cache cache) program transition))
        )
      );
      (
        "sizebound_local" >::: (
          [
            ("l1", "l2", Var.mk_arg 0, "0");
            (* TODO We have to redefine good bounds here: ("l1", "l1", "A", "A-1"); *)
          ]
        |> List.map (fun (l,l',var,bound) ->
               "Bound from " ^ l ^ " to " ^ l' ^ " for var " ^ Var.to_string var ^ " is " ^ bound ^ "?" >:: (fun _ ->
                       let cache = CacheManager.new_cache () in
                       let trans_id_counter = CacheManager.trans_id_counter cache in

                       let program = Readers.read_file trans_id_counter "../../examples/KoAT-2013/sect1-lin.koat" in
                       let t =
                         TransitionGraph.transitions (Program.graph program)
                         |> TransitionSet.filter (fun (l2,_,l2') -> Location.(name l2 = l && name l2' = l'))
                         |> TransitionSet.any
                       in
                       TransitionLabel.(assert_equal_bound
                                                  (Bound.of_poly (Readers.read_polynomial bound))
                                                  LocalSizeBound.(sizebound_local (CacheManager.lsb_cache cache) program `Upper t var |> Option.map as_bound |? default `Upper)
                       )
                     )
             )
        )

      );
      (
        let cache = CacheManager.new_cache () in
        let trans_id_counter = CacheManager.trans_id_counter cache in

        "Print" >:: (fun _ ->
          GraphPrint.print_system ~label:TransitionLabel.to_string ~outdir:(Fpath.v "output") ~file:"sect1-lin" (Readers.read_file trans_id_counter "../../examples/KoAT-2013/sect1-lin.koat");
          "../../examples/KoAT-2013/sect1-lin.koat"
          |> Readers.read_file trans_id_counter
          |> tap (fun program -> GraphPrint.print_rvg (CacheManager.pre_cache cache) (CacheManager.lsb_cache cache) `Lower ~label:RV.to_id_string ~outdir:(Fpath.v "output") ~file:"sect1-lin" program)
          |> tap (fun program -> GraphPrint.print_rvg (CacheManager.pre_cache cache) (CacheManager.lsb_cache cache) `Upper ~label:RV.to_id_string ~outdir:(Fpath.v "output") ~file:"sect1-lin" program)
          |> ignore
        )
      );
    ]
