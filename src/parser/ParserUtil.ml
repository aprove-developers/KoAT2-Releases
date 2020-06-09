(** Provides implemenation of transition and program generation. *)
open Batteries
open Formulas
open Polynomials
open ProgramTypes

exception Error of string

module LocationTable = Hashtbl.Make(Location)

(*Hashtable for caching the arities of locations while reading*)
let location_table: int LocationTable.t = LocationTable.create 20

let check_arity (loc: Location.t) (arity: int): unit = 
  let stored_arity = LocationTable.find_option location_table loc
  and string_of_stored_arities = location_table
                    |> LocationTable.enum
                    |> Util.enum_to_string (fun (location, arity) -> Location.to_string location ^ ": " ^ string_of_int arity) in
  match stored_arity with
    |None -> LocationTable.add location_table loc arity
    |(Some m) -> if (m == arity) then () else raise (Error ("Location " ^ (Location.to_string loc) ^" occurs with different arities of variables "^ (string_of_int m) ^ "<>" ^ (string_of_int arity) ^ " !" ^ "\n Stored arities: " ^ string_of_stored_arities))
   
(** Generates transitions from given parameters *)
let mk_transition lhs (cost: Polynomial.t) rhs (formula: Formula.t) (vars:Var.t list): Transition.t list =
  check_arity (Location.of_string (Tuple2.first lhs)) (List.length (Tuple2.second lhs));
  check_arity (Location.of_string (Tuple2.first (List.hd (Tuple2.second rhs)))) (List.length (Tuple2.second (List.hd (Tuple2.second rhs))));  
  formula
  |> Formula.constraints
  |> List.map (fun constr ->
	 (Location.of_string (Tuple2.first lhs),
          TransitionLabel.mk
            ~com_kind:(Tuple2.first rhs)
            ~targets:(Tuple2.second rhs)
            ~patterns:(List.map Var.of_string (Tuple2.second lhs))
            ~guard:constr 
            ~cost:cost,
          (Location.of_string (Tuple2.first (List.hd (Tuple2.second rhs)))))
       )
  |> List.map (fun (l,t,l') -> (l,t ~vars,l'))

(** Returns list of default variables: x,y,z,u,v,w,p and q *)
let default_vars =
  ["x"; "y"; "z"; "u"; "v"; "w"; "p"; "q"]
  |> List.map Var.of_string

(** Input is not interpreted as a filepath, but as a program in simple mode. Method returns all transitions from such an input. *)
let mk_transition_simple (start: string) (cost: Polynomial.t) (rhs: string * (string * Polynomial.t list) list) (formula: Formula.t): Transition.t list =
  formula
  |> Formula.constraints
  |> List.map (fun constr ->
	 (Location.of_string start,
          TransitionLabel.mk
            ~com_kind:(Tuple2.first rhs)
            ~targets:(Tuple2.second rhs)
            ~patterns:default_vars
            ~guard:constr 
            ~cost:cost
            ~vars:default_vars, (Location.of_string (Tuple2.first (List.hd (Tuple2.second rhs)))))
       )

(** Input is not interpreted as a filepath, but as a program in simple mode. Method returns program from such an input. *)
let mk_program_simple (transitions: Transition.t list): Program.t =
  transitions
  |> List.hd
  |> Transition.src
  |> Program.from transitions

(** Returns a program corresponding to the given list of transition with a fixed start location. *)
let mk_program goal start vars (transitions: Transition.t list): Program.t =
  Program.from transitions start
