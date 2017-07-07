(*Locations of integer transition systems, should end up in a system using ocamlgraph*)
type t = 
    {
        name : string;
(*         vars : Variables.t list; *)
(*        invariant : PolynomialConstraints.t*)
        
    }

let equal (l1 : t) (l2 : t) = (l1.name == l2.name) (*&& (Variables.equal_varlist l1.vars l2.vars)*)

let compare (l1 : t) (l2 : t) = 
    if (equal l1 l2) then 0
    else if l1.name < l2.name then (-1)
    else 1
(*Needed by ocamlgraph*)    
let hash (loc : t) = Hashtbl.hash (loc.name)

let to_string (loc : t) = String.concat "_" ["loc" ; (loc.name)]

let of_string ( inName : string ) = 
    {
        name = inName;
        
    }
    
let default = { name = "default" }