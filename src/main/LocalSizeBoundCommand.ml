open Batteries

let description = "Search for a local size bound"

let command = "lsb"
   
type params = {
    
    kind : [`Upper | `Lower]; [@enum ["upper", `Upper; "lower", `Lower]] [@pos 0] [@default `Upper]
    (** Which type of bound is requested. Available options: upper and lower. *)

    guard : string; [@default ""]
    (** The guard of the transition in the form of a constraint.
        That is a &&-separated list of atoms.
        Atoms are two polynomials in a relation with <, >, <=, >= or =. *)

    var : string; [@default "x"]
    (** The variable for which a local size bound should be found. *)

    update : string option;
    (** The polynomial to which the value of the variable gets updated after the transition. *)
    
  } [@@deriving cmdliner, show]

let run (params: params) =
  Logger.init ["lsb", Logger.DEBUG] (Logger.make_dbg_formatter IO.stdout);
  let open TransitionLabel in
  let guard = Readers.read_constraint params.guard in
  let var = Var.of_string params.var in
  let update = match params.update with
    | Some str -> VarMap.(add var (Readers.read_polynomial str) empty)
    | None -> VarMap.empty in
  let label = make "Com_1" ~start:"" ~target:"" ~update ~guard in
  print_string (Bound.to_string LocalSizeBound.(as_bound (sizebound_local params.kind label var)))

