open ProgramTypes

let test program = 
    print_string(program |> Program.generalized_transitions |> GeneralTransitionSet.to_string);
    print_string("\n")