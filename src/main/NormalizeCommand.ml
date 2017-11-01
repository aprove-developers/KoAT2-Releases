open Batteries
open Polynomials
open Atoms

let description = "Find a normalform for an input"

let command = "normalize"
   
type params = {

    kind : [`Atom | `Polynomial | `Bound]; [@enum ["atom", `Atom; "poly", `Polynomial; "bound", `Bound]] [@pos 0]  [@docv "KIND"]
    (** How the input should be interpreted. *)

    input : string; [@pos 1] [@docv "INPUT"]
    (** The input which should be normalized *)
    
  } [@@deriving cmdliner, show]

let run (params: params) =
  let output = match params.kind with
    | `Polynomial -> Polynomial.to_string (Polynomial.simplify (Readers.read_polynomial params.input))
    | `Atom -> Atom.to_string (Readers.read_atom params.input)
    | `Bound -> Bound.to_string (Readers.read_bound params.input) in
  print_string output
  
