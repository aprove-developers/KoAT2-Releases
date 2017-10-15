open Batteries

include Hashtbl.Make(Program.TransitionGraph.V)

type parameter_table = (Polynomials.ParameterPolynomial.t) t

type polynomial_table = (Polynomials.Polynomial.t) t

let to_string_parapoly hashtable =
  let output = IO.output_string () in
  print (fun output key -> IO.nwrite output (Program.Location.to_string key)) (fun output parapoly -> IO.nwrite output (Polynomials.ParameterPolynomial.to_string parapoly)) output hashtable;
  IO.close_out output

let to_string_poly hashtable =
  let output = IO.output_string () in
  print (fun output key -> IO.nwrite output (Program.Location.to_string key)) (fun output parapoly -> IO.nwrite output (Polynomials.Polynomial.to_string parapoly)) output hashtable;
  IO.close_out output