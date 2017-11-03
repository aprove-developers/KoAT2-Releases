open Batteries
open Polynomials
open Program.Types
   
include Hashtbl.Make(Location)

(* The default method from Hashtbl seems to be buggy *)
let of_list pairs =
  let hashtbl = create (List.length pairs) in
  let add_e (key, element) =
    add hashtbl key element in
  List.iter add_e pairs; hashtbl

let to_string poly_to_string hashtable =
  let output = IO.output_string () in
  print ~first:"[" ~last:"]" ~sep:", "
        (fun output key -> IO.nwrite output (Location.to_string key)) (fun output poly -> IO.nwrite output (poly_to_string poly)) output hashtable;
  IO.close_out output  

let to_string_parapoly =
  to_string ParameterPolynomial.to_string

let to_string_poly =
  to_string Polynomial.to_string
