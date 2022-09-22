(** Provides implemenation of transition and program generation. *)
open Batteries
open Formulas
open Polynomials
open Program

exception Error of string
exception ComKindAndTargetsMismatch
exception OnlyCom1InSimpleMode

module LocationTable = Hashtbl.Make(Location)

(** Hashtable for caching the arities of locations while reading **)
let location_table: int LocationTable.t = LocationTable.create 20

let empty_cache () =
  LocationTable.clear location_table

let check_arity (loc: Location.t) (arity: int): unit =
  let stored_arity = LocationTable.find_option location_table loc
  and string_of_stored_arities = location_table
                    |> LocationTable.enum
                    |> Util.enum_to_string (fun (location, arity) -> Location.to_string location ^ ": " ^ string_of_int arity) in
  match stored_arity with
    |None -> LocationTable.add location_table loc arity
    |(Some m) -> if (m == arity) then () else raise (Error ("Location " ^ (Location.to_string loc) ^" occurs with different arities of variables "^ (string_of_int m) ^ "<>" ^ (string_of_int arity) ^ " !" ^ "\n Stored arities: " ^ string_of_stored_arities))

(** Generates transitions from given parameters *)
let mk_transition lhs (cost: Polynomial.t) rhs (formula: Formula.t) (vars:Var.t list) =
  let targets = Tuple2.second rhs in
  let start_loc = Tuple2.first lhs in
  let var_list = Tuple2.second lhs in
  let com_kind = Int.of_string @@ String.lchop ~n:4 @@ Tuple2.first rhs in
  if not (Int.equal com_kind (List.length targets)) then raise ComKindAndTargetsMismatch else
    check_arity (Location.of_string start_loc) (List.length var_list);
    List.iter
      (fun (target_loc, update_expr) -> check_arity (Location.of_string target_loc) (List.length update_expr))
      (Tuple2.second rhs);
    formula
    |> Formula.constraints
    |> List.map (fun constr ->
        List.map
          (fun (target_loc, update_expr) ->
            (Location.of_string start_loc,
              TransitionLabel.mk ~assignments:update_expr ~guard:constr ~patterns:(List.map Var.of_string var_list ) ~cost,
              Location.of_string target_loc)
          )
          (Tuple2.second rhs)
       )
    |> List.map (List.map (fun (l,t,l') -> (l,t ~vars,l')))

(** Returns list of default variables: x,y,z,u,v,w,p and q *)
let default_vars =
  ["x"; "y"; "z"; "u"; "v"; "w"; "p"; "q"]
  |> List.map Var.of_string

(** Input is not interpreted as a filepath, but as a program in simple mode. Method returns all transitions from such an input. *)
(** Assume Com_1 transitions *)
let mk_transition_simple (start: string) (cost: Polynomial.t) (rhs: string * (string * Polynomial.t list) list) (formula: Formula.t): Transition.t list =
  let com_kind = Int.of_string @@ String.lchop ~n:4 @@ Tuple2.first rhs in
  if not (Int.equal com_kind 1) then raise OnlyCom1InSimpleMode else
    let (target_loc, updates) =  List.hd @@ Tuple2.second rhs in
    formula
    |> Formula.constraints
    |> List.map (fun constr ->
           (Location.of_string start,
            TransitionLabel.mk
              ~assignments:updates
              ~patterns:default_vars
              ~guard:constr
              ~cost:cost
              ~vars:default_vars, Location.of_string target_loc)
         )

(** Input is not interpreted as a filepath, but as a program in simple mode. Method returns program from such an input. *)
let mk_program_simple (transitions: Transition.t list): Program.t =
  transitions
  |> List.hd
  |> Transition.src
  |> Program.from (List.map List.singleton transitions)

(** Returns a program corresponding to the given list of transition with a fixed start location. *)
let mk_program goal start vars (transitions: Transition.t list): Program.t =
  Program.from (List.map List.singleton transitions) start
