open ProgramModules

include Hashtbl.Make (struct include Transition let equal = Transition.same end)
