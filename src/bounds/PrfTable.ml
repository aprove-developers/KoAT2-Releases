open Batteries

include Hashtbl.Make(Program.TransitionGraph.V)

type table = (Polynomials.ParameterPolynomial.t) t

let to_string hashtable =
  let output = IO.output_string () in
  print (fun output key -> IO.nwrite output (Program.Location.to_string key)) (fun output parapoly -> IO.nwrite output (Polynomials.ParameterPolynomial.to_string parapoly)) output hashtable;
  IO.close_out output
