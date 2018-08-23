open Batteries
open ProgramTypes
open Sys
open Unix
open Polynomials

let description = "Testing for finding roots of a polynomial"

let command = "roots"

type params = {
    input : string; [@aka ["i"]] [@pos 0]
    (** Polynomial in String form we want to get the roots from*)
    
  } [@@deriving cmdliner, show]

let read_process_lines command =
  let lines = ref [] in
  let in_channel = Unix.open_process_in command in
  begin
    try
      while true do
        lines := input_line in_channel :: !lines
      done;
    with BatInnerIO.Input_closed -> ()
  end;
  List.rev !lines

let rec filter_roots = function
  | (_,[]) -> []
  | (k, x::xs) -> if String.equal x k then xs else filter_roots (k, xs)

let make_test_polynomial coeff_string =
  coeff_string
  |> String.split_on_char ' '
  |> List.map int_of_string
  |> List.mapi (fun c x -> (Polynomial.of_power (Var.of_string "X") c, Polynomial.of_int x))
  |> List.map (fun (x,c) -> Polynomial.mul x c)
  |> List.fold_left Polynomial.add Polynomial.zero
  |> Polynomial.simplify

let rec make_deg_list = function
  | 0 -> 0::[]
  | x -> if x<0 then [] else (x)::make_deg_list (x-1)

let poly_coeffs poly =
  let var = poly |> Polynomial.vars |> VarSet.any in
  let monos = poly 
              |> Polynomial.degree 
              |> make_deg_list 
              |> List.rev 
              |> List.map (Polynomial.of_power var) 
              |> List.map Polynomial.monomials 
              |> List.map List.hd in 
  List.map (fun mono -> Polynomial.coeff mono poly) monos

let coeff_list_to_string list =
  list
  |> List.map OurInt.to_string
  |> String.concat " "
  |> fun s -> "[" ^ s ^ "]"

let get_roots poly =
  poly
  |> poly_coeffs
  |> coeff_list_to_string
  |> fun s -> read_process_lines ("matlab -nodesktop -nojvm -r 'format compact; format long; r=roots(" ^ s ^ ");r=r(~imag(r)),exit'")
  |> fun list -> filter_roots ("r =", list)
  |> fun list -> List.take (List.length list - 1) list
  |> List.map String.trim
  |> List.map OurFloat.of_string

let run (params: params) =
  Logging.(use_loggers [Roots, Logger.DEBUG]);
  let logger = Logging.(get Roots) in
  let execute () =
    params.input
    |> make_test_polynomial
    |> get_roots
  in 
  Logger.with_log logger Logger.DEBUG 
                  (fun () -> "find roots", [])
                  ~result:(fun roots ->
                    roots
                    |> List.map OurFloat.to_string
                    |> String.concat ", "
                  )
                  execute
  |> ignore