open Batteries
open OUnit2
open Koat2
open ProgramModules

let tests =
  "ProgrammTest" >::: [
    "pre" >:::
      List.map (fun (program_simple_str, has_pre) ->
        let program = Readers.read_program_simple program_simple_str in
        let trans =
          Program.transitions program
          |> TransitionSet.filter (not % Location.equal (Program.start program) % Transition.src)
          |> TransitionSet.any
        in
        program_simple_str >:: fun _ ->
          assert_bool
            (let pre_string =
               if has_pre then " to have a pre transition"
               else " to not have a pre transition"
             in "Expected " ^ Transition.to_id_string trans ^ pre_string ^ ", however the opposite was computed."
            )
            (has_pre = (1 = List.length (List.of_enum @@ Program.pre program trans)))
      )

      [
        "a -> b(), b -> c()", true
      ; "a -> b(3), b -> c() :|: x>0", true
      ; "a -> b(3), b -> c() :|: x<0", false

      ; "a -> b(Temp), b -> c() :|: x<0", true
      ; "a -> b(Temp) :|: Temp > 0, b -> c() :|: x<0", false
      ; "a -> b() :|: Temp > 0, b -> c() :|: Temp<0", true
      ; "a -> b() :|: Temp > 0, b -> c() :|: Temp>0", true
      ; "a -> b(Temp) :|: Temp > 0, b -> c() :|: x<0", false
      ; "a -> b(Temp) :|: Temp > 0, b -> c() :|: x<0", false
      ; "a -> b(Temp) :|: Temp > 0, b -> c() :|: Temp<0", true

      ; "a -> b(y), b -> c() :|: x<y", false
      ; "a -> b(y), b -> c() :|: x<=y", true
      ]
  ]
