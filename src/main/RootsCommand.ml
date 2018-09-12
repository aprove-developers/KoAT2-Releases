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
    with 
      | BatInnerIO.Input_closed -> ()
      | End_of_file -> ()
  end;
  List.rev !lines

let rec filter_roots = function
  | (_,[]) -> []
  | (k, x::xs) -> if String.equal x k then xs else filter_roots (k, xs)

let make_test_polynomial coeff_string =
  coeff_string
  |> String.split_on_char ' '
  |> List.rev
  |> List.map int_of_string
  |> List.mapi (fun c x -> (Polynomial.of_power (Var.of_string "X") c, Polynomial.of_int x))
  |> List.map (fun (x,c) -> Polynomial.mul x c)
  |> List.fold_left Polynomial.add Polynomial.zero
  |> Polynomial.simplify

let make_real_polynomial coeff_string =
  coeff_string
  |> String.split_on_char ' '
  |> List.rev
  |> List.map OurFloat.of_string
  |> List.mapi (fun c x -> (RealPolynomial.of_power (Var.of_string "X") c, RealPolynomial.of_constant x))
  |> List.map (fun (x,c) -> RealPolynomial.mul x c)
  |> List.fold_left RealPolynomial.add RealPolynomial.zero
  |> RealPolynomial.simplify

let coeff_list_to_string_matlab list =
  list
  |> List.rev
  |> List.map OurInt.to_string
  |> String.concat " "
  |> fun s -> "[" ^ s ^ "]"

let get_roots_matlab poly =
  poly
  |> Polynomial.degree_coeff_list
  |> coeff_list_to_string_matlab
  |> fun s -> read_process_lines ("matlab -nodesktop -nojvm -r 'format compact; format long; r=roots(" ^ s ^ ");r=r(~imag(r)),exit'")
  |> fun list -> filter_roots ("r =", list)
  |> fun list -> List.take (List.length list - 1) list
  |> List.map String.trim
  |> List.map OurFloat.of_string

let coeff_list_to_string_sage list =
  list
  |> List.mapi (fun n k -> (OurInt.to_string k) ^ "*x^" ^ (string_of_int n))
  |> String.concat "+"

let real_coeff_list_to_string_sage list =
  list
  |> List.mapi (fun n k -> (OurFloat.to_string k) ^ "*x^" ^ (string_of_int n))
  |> String.concat "+"

let get_roots_sage poly =
  let real_roots = 
  poly
  |> Polynomial.degree_coeff_list
  |> coeff_list_to_string_sage
  |> fun s -> read_process_lines ("sage -c 'x = polygen(ZZ); print((" ^ s ^ ").real_roots())'")
  |> List.hd 
  |> String.chop
  |> (fun rootstr -> String.nsplit rootstr ", ")
  |> List.map float_of_string
  in
  real_roots

let create_script c poly_string =
  ["x = polygen(ZZ)";
  "poly = (" ^ poly_string ^ ")";
  "drift = -0.6";
  "c = " ^ string_of_float c ;
  "roots = sage.rings.polynomial.complex_roots.complex_roots(poly)";
  "# this is super dirty, since we just cast the approximation to a float.";
  "# maybe there is a cleaner solution, but this will have to do for now.";
  "roots = [CC(x[0]) for x in roots if RR(abs(x[0])) <= 1.]";
  "real_roots = []";
  "for root in roots:";
  "    if root.imag() != 0:";
  "        real_roots.append(root.real())";
  "        real_roots.append(root.imag())";
  "        if conjugate(root) in roots:";
  "            roots.remove(conjugate(root))";
  "    else:";
  "        real_roots.append(RR(root))";
  "k = len(real_roots)";
  "A = matrix([[x^(-i) for x in real_roots] for i in range(k)])";
  "B = vector([c*i for i in range(k)])";
  "solution = A.solve_right(B)";
  "for sol in solution:";
  "    print(sol)"]


let test c poly =
  let script = 
  poly
  |> RealPolynomial.degree_coeff_list
  |> real_coeff_list_to_string_sage
  |> create_script c
  in
  script
  |> List.enum
  |> File.write_lines "tmp_sage_script.sage"



let run (params: params) =
  Logging.(use_loggers [Roots, Logger.DEBUG]);
  let logger = Logging.(get Roots) in
  let execute () =
    "0.2 0 0.4 0 0.2 0.2"
    |> make_real_polynomial
    |> test 1.6666666666666667;
    read_process_lines "sage tmp_sage_script.sage"
    |> List.map float_of_string
    |> List.map print_float
    |> ignore;
    Unix.unlink "tmp_sage_script.sage";
    Unix.unlink "tmp_sage_script.sage.py";
    params.input
    |> make_test_polynomial
    |> get_roots_sage
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