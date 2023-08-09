open Koat2
open Batteries
open OUnit2
open Helper

let tests =
  "VarSet"
  >::: [
         "powerset"
         >::: List.map
                (fun (expected, varset) ->
                  VarSet.to_string varset >:: fun _ ->
                  assert_equal_varset_enum expected
                    (List.enum @@ Base.Sequence.to_list @@ OurBase.Set.powerset (module Var) varset))
                [
                  (List.enum [], VarSet.empty);
                  (List.enum [ VarSet.empty ], VarSet.empty);
                  (List.enum [ VarSet.empty; VarSet.of_string_list [ "x" ] ], VarSet.of_string_list [ "x" ]);
                  ( List.enum
                      [
                        VarSet.empty;
                        VarSet.of_string_list [ "x" ];
                        VarSet.of_string_list [ "y" ];
                        VarSet.of_string_list [ "x"; "y" ];
                      ],
                    VarSet.of_string_list [ "x"; "y" ] );
                ];
       ]
