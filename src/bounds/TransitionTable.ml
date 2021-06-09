open ProgramTypes

include Hashtbl.Make (struct include Transition let equal = Transition.same end)
