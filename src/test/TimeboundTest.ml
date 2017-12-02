open Batteries
open OUnit2
open Helper
open Program.Types

(** Returns an overall timebound for the given program. *)
let find_timebound (program: Program.t): Bound.t =
  (program, Approximation.create program)
  |> Preprocessor.process_til_fixpoint Preprocessor.all
  |> (fun (program, appr) ->
    Bounds.find_bounds program appr
    |> fun appr -> Approximation.(Time.sum (time appr) program)
  )
  
(** Shouldnt be here *)
module M = Map.Make(Var)
let from entries =
  let addEntry entry = match entry with
    | (key, value) -> M.add key value in
  List.fold_left (fun map keyadder -> keyadder map) M.empty (List.map addEntry entries)
  
let is_bound_between (vars: VarSet.t) bound lower upper =
  [-999999;0;999999]
  |> Enum.repeat ~times:(VarSet.cardinal vars)
  |> List.of_enum
  |> List.n_cartesian_product
  |> List.for_all (fun assignment ->
         let valuation =
           assignment
           |> List.map Bound.of_int
           |> List.combine (VarSet.to_list vars)
           |> from
           |> (fun map var -> M.find var map)
         in
         (
           Bound.(substitute_f valuation bound <= substitute_f valuation upper) |? false
           && Bound.(substitute_f valuation lower <= substitute_f valuation bound) |? false
         )
       )
  
let tests = 
  "Overall timebound" >::: [
      
      ("Simple" >:::
         List.map (fun (minimal_sound_timebound, wanted_timebound, program_str) ->
             program_str >:: (fun _ ->
                     let program = Readers.read_program_simple program_str in
                     let timebound = find_timebound program in
                     assert_bool ("Bound not fine: " ^ Bound.to_string timebound) (is_bound_between (Program.vars program) timebound minimal_sound_timebound wanted_timebound)))
                  [
                    (* Constant bound *)
                    (Bound.one, Bound.one, "l1 -> l2(x)");
                    (Bound.of_int 2, Bound.of_int 2, "l1 -> l2(x), l2 -> l3(x)");
                    (Bound.of_int 2, Bound.of_int 2, "l1 -> l2(x), l2 -> l3(x), l2 -> l4(x)");
                    (Bound.of_int 3, Bound.of_int 3, "l1 -> l2(x), l2 -> l3(x), l2 -> l4(x), l3 -> l4(x)");
                    (Bound.of_int 11, Bound.of_int 11, "l1 -> l2(10), l2 -> l2(x-1) :|: x>0");
                    (Bound.of_int 6, Bound.of_int 6, "l1 -> l2(10), l2 -> l2(x-2) :|: x>0");
                    (* Linear bound *)
                  ]
      );
      
    ]
