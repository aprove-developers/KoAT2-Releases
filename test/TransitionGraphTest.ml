open Koat2
open Batteries
open Bounds
open OUnit2
open Helper
open ProgramModules
open RVGTypes

let suite =
  "Graphs"
  >::: [
         (let test_folder folder =
            "examples_old_input/" ^ folder ^ "/"
            >:::
            let files =
              Array.filter (fun s -> String.ends_with s ".koat") (Sys.readdir ("../../../examples/" ^ folder))
            and test (file : string) : unit =
              try ignore (Readers.read_file ("../../../examples/" ^ folder ^ "/" ^ file)) with
              | ParserUtil.Error msg -> failwith msg
              | Program.RecursionNotSupported -> skip_if true "Recursion not supported"
            in
            Array.to_list (Array.map (fun s -> s >:: fun _ -> test s) files)
          in
          "Examples" >::: List.map test_folder [ "KoAT-2013"; "KoAT-2014"; "SAS10"; "T2" ]);
         (let test_folder folder =
            "examples_new_input/" ^ folder ^ "/"
            >:::
            let files =
              Array.filter (fun s -> String.ends_with s ".koat") (Sys.readdir ("../../../examples/" ^ folder))
            in
            let test (file : string) : unit =
              try
                let read_function () =
                  ignore (Readers.read_file ("../../../examples/" ^ folder ^ "/" ^ file))
                in
                (* Check if an exception is raised when transitions lead back to the initial location *)
                if file = "start_incoming.koat" then
                  assert_raises (Failure "Transition leading back to the initial location.") read_function
                else
                  read_function ()
              with
              | ParserUtil.Error msg -> failwith msg
              | Program.RecursionNotSupported -> skip_if true "Recursion not supported"
            in
            Array.to_list (Array.map (fun s -> s >:: fun _ -> test s) files)
          in
          "Examples"
          >::: List.map test_folder
                 [
                   "CageKoAT-Input-Examples/weightedExamples/badExamples";
                   "CageKoAT-Input-Examples/weightedExamples/constantWeights";
                   "CageKoAT-Input-Examples/weightedExamples/simplePolyWeights";
                   "CageKoAT-Input-Examples/debugExamples";
                   "CageKoAT-Input-Examples/cexamples";
                 ]);
         ( "pre(t)" >:: fun _ ->
           let program = Readers.read_file "../../../examples/KoAT-2013/sect1-lin.koat" in
           let transition =
             TransitionGraph.find_edge (Program.graph program) (Location.of_string "l1")
               (Location.of_string "l2")
           in
           assert_equal_int 2 (Base.Set.length (Program.pre program transition)) );
         ("sizebound_local"
         >:::
         let arg0 = Base.Sequence.hd_exn Var.args in
         [
           ("l1", "l2", arg0, Bound.of_var arg0);
           (* TODO We have to redefine good bounds here: ("l1", "l1", "A", "A-1"); *)
         ]
         |> List.map (fun (l, l', var, bound) ->
                "Bound from " ^ l ^ " to " ^ l' ^ " for var " ^ Var.to_string var ^ " is "
                ^ Bound.to_string bound ^ "?"
                >:: fun _ ->
                let program = Readers.read_file "../../../examples/KoAT-2013/sect1-lin.koat" in
                let t =
                  TransitionGraph.find_edge (Program.graph program) (Location.of_string l)
                    (Location.of_string l')
                in
                assert_equal_bound bound LocalSizeBound.(sizebound_local program t var |> option_lsb_as_bound))
         );
         ( "Print" >:: fun _ ->
           GraphPrint.print_system ~format:"png"
             ~label:(TransitionLabel.to_string % Transition.label)
             ~outdir:(Fpath.v "output") ~file:"sect1-lin"
             (Readers.read_file "../../../examples/KoAT-2013/sect1-lin.koat");
           "../../../examples/KoAT-2013/sect1-lin.koat" |> Readers.read_file
           |> tap (fun program ->
                  GraphPrint.print_rvg ~format:"png" ~label:RV.to_id_string ~outdir:(Fpath.v "output")
                    ~file:"sect1-lin" program)
           |> ignore );
         "ParseEdgeCases"
         >::: [
                (let program_str =
                   "(GOAL COMPLEXITY)\n(STARTTERM (FUNCTIONSYMBOLS f0))\n(VAR A)\n(RULES f0(X,A)->f1(A,A))"
                 in
                 let prog = Readers.read_program program_str in
                 program_str >:: fun _ ->
                 Base.Set.choose_exn (Program.transitions prog)
                 |> TransitionLabel.update_map % Transition.label
                 |> Base.Map.for_all ~f:(fun u ->
                        Polynomials.Polynomial.(equal (of_var @@ Var.of_string "Arg_1") u))
                 |> assert_bool "Wrongly parsed update");
              ];
       ]
