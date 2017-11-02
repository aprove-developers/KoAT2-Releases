open Batteries
open Polynomials
open Program.Types
   
include Hashtbl.Make(TransitionGraph.V)

type parameter_table = ParameterPolynomial.t t

type polynomial_table = Polynomial.t t

let to_string_parapoly hashtable =
  let output = IO.output_string () in
  print (fun output key -> IO.nwrite output (Location.to_string key)) (fun output parapoly -> IO.nwrite output (ParameterPolynomial.to_string parapoly)) output hashtable;
  IO.close_out output

let to_string_poly hashtable =
  let output = IO.output_string () in
  print (fun output key -> IO.nwrite output (Location.to_string key)) (fun output parapoly -> IO.nwrite output (Polynomial.to_string parapoly)) output hashtable;
  IO.close_out output
