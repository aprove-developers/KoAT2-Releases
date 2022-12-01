open Automorphism
open Batteries
open ProgramTypes
open Formulas
open Polynomials
open ProgramModules

module SMTSolver = SMT.Z3Solver
module SMTSolverTimeout = SMT.Z3SolverTimeout

module ScaledMonomial = ScaledMonomials.Make(OurInt)
module Monomial = Monomials.Make(OurInt)

module TWNLoop = TWNLoop.Make(ProgramModules)
module Check_Solvable = Check_Solvable.Make(ProgramModules)

type transformation_type = TransformTryBoth
                         | GeneralTransform
                         | JordanTransform   [@@deriving eq]

(** Currently the TWN transformations *only* work on ProgramModules.Program.t . *)
(** The reason is the underlying program itself is transformed rather than a possible TWNLoop that was abstracted from the program *)

(** get_linear_update_of_variable (x<- 2x+3y+y^2) x y returns 3 *)
let get_linear_update_of_variable (t:TWNLoop.t) (var_left:VarMap.key) (var_right:VarMap.key)=
  match (TWNLoop.update t var_left) with
    | None -> (OurInt.of_int 0)
    | Some update ->
  let fupdate = List.filter (fun (x,y) -> Monomial.is_univariate_linear y) (Polynomial.monomials_with_coeffs update) in
  let (x,y) = List.find  (fun (x,y) -> (Var.equal (List.first (VarSet.to_list (Monomial.vars y))))  var_right) fupdate in
  x

(** get_linear_update_list [(x<- 2x+3y+y^2) x [x;y]] returns [[2;3]] *)
let rec get_linear_update_list (t:TWNLoop.t) (var_left:VarMap.key) (block:VarMap.key list) = match block with
  | [] -> []
  | x::xs -> get_linear_update_of_variable t var_left x :: get_linear_update_list t var_left xs

let matrix_of_linear_assignments (t:TWNLoop.t) (block:VarMap.key list) =
  List.map (fun x -> get_linear_update_list t x block) block


(** [matrix_times_vector A x] returns a polynomial list where each element stores a row of [A*x] *)
let matrix_times_vector_rational (matrix:OurRational.t list list) (vars:VarMap.key list) =
  List.map (fun xs -> RationalPolynomial.of_coeff_list xs vars) matrix

let matrix_times_vector_int (matrix:OurInt.t list list) (vars:VarMap.key list) =
  List.map (fun xs -> Polynomial.of_coeff_list xs vars) matrix

(** sorts the blocks from function check_solvable in the order defined in the transition (needs O(n^2 log n) due to index_of) *)
let change_order t blocks =
  let var_list = (VarSet.to_list (TWNLoop.vars t)) in
  let blocks = List.map (List.sort (fun x y ->
                                            if List.index_of x var_list < List.index_of y var_list then -1 else 1)
                        ) blocks in
  List.sort (fun x y -> if List.index_of (List.first x) var_list < List.index_of (List.first y) var_list then -1 else 1) blocks

let read_process_lines command = (* This function was written by Tom Küspert *)
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

(** returns python-friendly list (not list of list) of [matrix]*)
let matrix_to_string matrix =
  let flattened_matrix = List.flatten matrix in
  "["
  ^ List.fold (fun x y -> x  ^ (OurInt.to_string y)^ ",") "" (List.take (List.length flattened_matrix -1) flattened_matrix)
  ^ (OurInt.to_string (List.last flattened_matrix))
  ^ "]"

(** separates [xs] into several list of length n. If [n] does not divide the length of the list, the last chunk will be shorter*)
let rec list_lift (n:int) (xs:'a list) : 'a list list = match  xs with
    | [] -> []
    | xs -> let (a,b) = List.takedrop n xs in a::(list_lift n b)


(** turns string into OurInt list list (which represents a matrix).
  [parse_matrix d s] return the matrix of dimension [d] times [d] that is described with string [s] *)
let parse_int_matrix (dim:int) (s:string) =
  Str.split (Str.regexp "[^0-9/./\\-]+") s (*turn string into list, splitted at each non number char *)
      |> List.map int_of_string
      |> List.map OurInt.of_int
      |> list_lift dim

(** [parse_matrix d s] return the matrix of dimension [d] times [d] that is described with string [s] *)
let parse_matrix (dim:int) (s:string) =
  Str.split (Str.regexp "[^0-9/./\\-]+") s (*turn string into list, splitted at each non number character *)
      |> List.map OurRational.of_string
      |> list_lift dim  (*turn OurRational list into OurRational list list *)


(**[transform_linearly_matrix A] returns the jordan decomposition [(T * B *T^{-1})] of [A] if all eigenvalues are integer and
Here, [T^{-1}] is normalized, i.e., an integer matrix.
[A] has to be square *)
let transform_linearly_matrix (matrix: OurInt.t list list) =
  if List.length matrix == 1 (* nothing to transform *)
      then Some ( [[(OurInt.of_int 1, OurInt.of_int 1)]], matrix,  [[OurInt.of_int 1]])
  else
    let command = "python3 -c 'from src.bounds.Solvable.JordanNormalForm import jordan_normal_form; jordan_normal_form(" ^ matrix_to_string matrix ^ ")'" in
    (* the python output consits of 3 matrices: T, J, T^-1 (which is normalized, see jordan normal form) or gives an error string *)
    let python_output = read_process_lines command in
    match python_output with
      | [a;b;c] ->    Some(parse_matrix (List.length matrix) a,
                        	 parse_int_matrix (List.length matrix) b,
                           parse_int_matrix (List.length matrix) c)
      | _ -> None (*error string *)

(**[transform_with_aut twn_loop automorphism vars] transforms a loop by the given automorphism,
i.e., applies the inverse to the invariant and guard and transforms the update   *)
let transform_with_aut twn_loop automorphism vars =
  match VarMap.map RationalPolynomial.of_intpoly @@ TWNLoop.update_map twn_loop (*current update *)
                  |> Automorphism.transform_update automorphism with
                  |  None -> None
                  |  Some x -> let  new_update = List.map Polynomial.simplify x in
  let updated_guard =  Formula.atoms @@ TWNLoop.guard_without_inv twn_loop (*current guard *)
                    |> Automorphism.transform_guard automorphism in
  let updated_invariant = Automorphism.transform_guard automorphism @@ TWNLoop.invariant twn_loop in
  let new_transitionlabels = TWNLoop.subsumed_transitionlabels twn_loop
                             |> List.map (fun t -> TransitionLabel.mk
                            ~id:(TransitionLabel.id t |> Option.some)
                            ~cost:(TransitionLabel.cost TransitionLabel.default) (*TODO sind die kosten von bedeutung? *)
                            ~guard:updated_guard
                            ~assignments:new_update
                            ~patterns:vars
                            ~vars:(VarSet.to_list (TWNLoop.vars twn_loop))) in
  (*return updated transition and automorphism*)
  Some (TWNLoop.mk_transitions new_transitionlabels
        |> (flip TWNLoop.add_invariant) updated_invariant )

(** [transform_linearly loop transformation_type] first checks whether a loop is in twn-form.
    Then tries to find a  transformation using the jordan decomposition. To compute it, we call sympy.*)
let transform_linearly (transition: TWNLoop.t) transformation_type =
  (* find order for variables and independent blocks and sort them for elegant code when updating transition*)
  match Check_Solvable.check_solvable transition with
    | None -> None
    | Some x ->
      let blocks = change_order transition x in
      if List.length blocks == List.length (VarSet.to_list (TWNLoop.vars transition)) then
          Some (transition, Automorphism.identity_aut) (*loop already is in twn form*)
      else if transformation_type != JordanTransform  && transformation_type != TransformTryBoth  then
        None
      else
        let concat_blocks = List.concat blocks in
        (* compute linear update matrices *)
        let matrices = List.map (matrix_of_linear_assignments transition) blocks in
        let transformations = List.map (transform_linearly_matrix) matrices in (*(transformations,transformed_matrices) *)
        if not @@ List.for_all Option.is_some transformations then
          None
        else
          let (transformations,js,transformation_invs) =
            List.fold_right (fun  (x1,x2,x3) (xs1,xs2,xs3)-> ((x1::xs1) , (x2::xs2) ,(x3::xs3)))
                      (List.map Option.get transformations)
                      ([],[],[]) in
          let eta = List.concat @@ List.map2 matrix_times_vector_rational transformations blocks in
          let eta_inv = List.concat @@ List.map2 matrix_times_vector_int transformation_invs blocks in
          let automorphism = Automorphism.of_poly_list concat_blocks eta eta_inv in
          match transform_with_aut transition automorphism concat_blocks with
          | None -> None
          | Some x -> Some(x, automorphism)

(** transform_non_linearly transforms a TWNLoop into twn form, it starts with the degree 1 and then counts upwards,
    until for some degree the formulas get too large. The degree of the inverse is the same as the degree of the automorphism
*)
let rec transform_non_linearly ?(degree =1) (t: TWNLoop.t) =
  let vars = VarSet.to_list @@ TWNLoop.vars t in
  try let endomorphism = Endomorphism.of_degree vars degree in
      let inv_formula = Endomorphism.formula_to_check_invertibility endomorphism in
      let twn_formula = Endomorphism.formula_to_check_twn vars endomorphism (TWNLoop.update_map t) in
      match SMTSolverTimeout.get_model @@ Formula.simplify @@ Formula.mk_and inv_formula twn_formula with
        | None ->  transform_non_linearly ~degree:(degree +1) t
        | Some valuation -> let automorphism = Automorphism.of_endomorphism endomorphism valuation in
          match transform_with_aut t automorphism vars with
          | None -> None (* transform with aut returns None iff result is not integer update, however this should never be the case as here we use integer automorphisms  *)
          | Some x -> Some (x, automorphism)
  with | Stack_overflow -> None (* TODO Waiting for the stack overflow to occur might not be the best idea *)

(** transform tries to transform a given loop into twn-form. First checks whether the given loop is twn;
then tries the Jordan approach;
then tries general approach
Returns the resulting twn-loop and an transformation automorphism *)
let transform transformation_type ((entry, x, t): Transition.t * (Transition.t list * Transition.t list) * TWNLoop.t)  =
   match transform_linearly t transformation_type with
  | Some (transformed, automorphism) ->  Some (entry, x, transformed, automorphism)
  | None -> if transformation_type ==  GeneralTransform || transformation_type ==  TransformTryBoth then
              match transform_non_linearly t with
                    | Some (transformed, automorphism) -> Some (entry, x, transformed, automorphism)
                    | None -> None
            else
              None
