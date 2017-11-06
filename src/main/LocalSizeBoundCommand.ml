open Batteries

let description = "Search for a local size bound"

let command = "lsb"
   
type params = {
    
    kind : [`Upper | `Lower]; [@enum ["upper", `Upper; "lower", `Lower]] [@pos 0] [@default `Upper]
    (** Which type of bound is requested. Available options: upper and lower. *)

    guard : string; [@default ""]
    (** The guard of the transition in the form of a constraint.
        That is a formula with and-separators (&&) and or-separators (||).
        Atoms are two polynomials in a relation with <, >, <=, >= or =. *)

    var : string; [@default "x"]
    (** The variable for which a local size bound should be found. *)

  } [@@deriving cmdliner, show]

let run (params: params) =
  Logger.init ["lsb", Logger.DEBUG] (Logger.make_dbg_formatter IO.stdout);
  let open TransitionLabel in
  let guard = Readers.read_formula params.guard in
  let var = Var.of_string params.var in
  print_string (Bound.to_string LocalSizeBound.(as_bound (find_bound var guard)))

