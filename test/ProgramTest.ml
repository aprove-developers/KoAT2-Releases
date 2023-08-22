open Koat2
open! OurBase
open OUnit2
open ProgramModules

let tests =
  "ProgrammTest"
  >::: [
         "pre"
         >::: List.map
                ~f:(fun (program_simple_str, has_pre) ->
                  let program = Readers.read_program_simple program_simple_str in
                  let trans =
                    Program.transitions program
                    |> Set.filter ~f:(not % Location.equal (Program.start program) % Transition.src)
                    |> Set.choose_exn
                  in
                  program_simple_str >:: fun _ ->
                  assert_bool
                    (let pre_string =
                       if has_pre then
                         " to have a pre transition"
                       else
                         " to not have a pre transition"
                     in
                     "Expected " ^ Transition.to_id_string trans ^ pre_string
                     ^ ", however the opposite was computed.")
                    (has_pre = (1 = Set.length (Program.pre program trans))))
                [
                  ("a -> b(), b -> c()", true);
                  ("a -> b(3), b -> c() :|: x>0", true);
                  ("a -> b(3), b -> c() :|: x<0", false);
                  ("a -> b(Temp), b -> c() :|: x<0", true);
                  ("a -> b(Temp) :|: Temp > 0, b -> c() :|: x<0", false);
                  ("a -> b() :|: Temp > 0, b -> c() :|: Temp<0", true);
                  ("a -> b() :|: Temp > 0, b -> c() :|: Temp>0", true);
                  ("a -> b(Temp) :|: Temp > 0, b -> c() :|: x<0", false);
                  ("a -> b(Temp) :|: Temp > 0, b -> c() :|: x<0", false);
                  ("a -> b(Temp) :|: Temp > 0, b -> c() :|: Temp<0", true);
                  ("a -> b(y), b -> c() :|: x<y", false);
                  ("a -> b(y), b -> c() :|: x<=y", true);
                ];
         "pre_cache"
         >::: List.map
                ~f:(fun program_simple_str ->
                  let program = Readers.read_program_simple program_simple_str in
                  let program_transitions = Program.transitions program in

                  let all_locations_without_start =
                    Set.remove (Program.locations program) (Program.start program)
                  in

                  let compare_pre_vs_pre_recomputed program name =
                    let check_trans trans =
                      let pre_recomputed =
                        TransitionSet.of_sequence @@ Program.InternalTest.compute_pre program trans
                      in
                      let pre = Program.pre program trans in
                      Transition.to_id_string trans >:: fun _ ->
                      assert_bool
                        ("pre " ^ TransitionSet.to_id_string pre ^ " pre_recomputed: "
                        ^ TransitionSet.to_id_string pre_recomputed)
                        (Set.equal pre_recomputed pre)
                    in
                    name >::: List.map ~f:check_trans (Set.to_list @@ Program.transitions program)
                  in

                  let check_after_remove_location locations_to_cut =
                    let program' = Set.fold ~init:program ~f:Program.remove_location locations_to_cut in
                    let program'_transitions = Program.transitions program' in

                    let removed_transitions = Set.diff program_transitions program'_transitions in
                    let program'' =
                      Program.map_graph
                        (TransitionGraph.add_transitions (Set.to_sequence removed_transitions))
                        program'
                    in
                    [
                      compare_pre_vs_pre_recomputed program'
                        ("after_remove_location_" ^ LocationSet.to_string locations_to_cut);
                      compare_pre_vs_pre_recomputed program''
                        ("after_remove_location_and_readd_location_" ^ LocationSet.to_string locations_to_cut);
                    ]
                  in

                  let check_after_remove_transition transitions_to_cut =
                    let program' = Set.fold ~init:program ~f:Program.remove_transition transitions_to_cut in
                    let program'_transitions = Program.transitions program' in

                    let removed_transitions = Set.diff program_transitions program'_transitions in
                    let program'' =
                      Program.map_graph
                        (TransitionGraph.add_transitions (Set.to_sequence removed_transitions))
                        program'
                    in
                    [
                      compare_pre_vs_pre_recomputed program'
                        ("after_remove_transition_" ^ TransitionSet.to_id_string transitions_to_cut);
                      compare_pre_vs_pre_recomputed program''
                        ("after_remove_transition_and_readd_transition_"
                        ^ TransitionSet.to_string transitions_to_cut);
                    ]
                  in

                  let mk_false_guard = Guard.mk_eq UpdateElement.zero UpdateElement.one in

                  let after_invalidating_guards transition_with_guards_to_invalidate =
                    let program' =
                      Program.map_labels
                        (fun label ->
                          if
                            Set.exists transition_with_guards_to_invalidate
                              ~f:(TransitionLabel.equal label % Transition.label)
                          then
                            TransitionLabel.map_guard (Guard.mk_and mk_false_guard) label
                          else
                            label)
                        program
                    in
                    [
                      compare_pre_vs_pre_recomputed program'
                        ("after_invalidating_guard_"
                        ^ TransitionSet.to_id_string transition_with_guards_to_invalidate);
                    ]
                  in

                  let after_invalidating_invariants locations_with_invariants_to_invalidate =
                    let program' =
                      Set.fold ~init:program
                        ~f:(fun prog loc -> Program.add_invariant loc mk_false_guard prog)
                        locations_with_invariants_to_invalidate
                    in
                    [
                      compare_pre_vs_pre_recomputed program'
                        ("after_invalidating_invariants_"
                        ^ LocationSet.to_string locations_with_invariants_to_invalidate);
                    ]
                  in

                  program_simple_str
                  >::: List.join
                         [
                           [ compare_pre_vs_pre_recomputed program "initial" ];
                           List.join
                           @@ Sequence.to_list
                                (Sequence.map ~f:check_after_remove_location
                                   (LocationSet.powerset all_locations_without_start));
                           List.join
                           @@ Sequence.to_list
                                (Sequence.map ~f:check_after_remove_transition
                                   (TransitionSet.powerset program_transitions));
                           List.join
                           @@ Sequence.to_list
                                (Sequence.map ~f:after_invalidating_guards
                                   (TransitionSet.powerset program_transitions));
                           List.join
                           @@ Sequence.to_list
                                (Sequence.map ~f:after_invalidating_invariants
                                   (LocationSet.powerset @@ Program.locations program));
                         ])
                [ "a -> b(), b -> c(), c -> c(), c -> d(), d -> c()" ];
       ]
