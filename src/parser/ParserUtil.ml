open Batteries
open Formulas
open Polynomials
open ProgramTypes
   
let mk_transition lhs (cost: Polynomial.t) rhs (formula: Formula.t) (vars:Var.t list): Transition.t list =
  formula
  |> Formula.constraints
  |> List.map (fun constr ->
	 (Location.of_string (Tuple2.first lhs),
          TransitionLabel.mk
            ~com_kind:(Tuple2.first rhs)
            ~targets:(Tuple2.second rhs)
            ~patterns:(List.map Var.of_string (Tuple2.second lhs))
            ~guard:constr 
            ~cost:cost
            ~probability:1.0,
          (Location.of_string (Tuple2.first (List.hd (Tuple2.second rhs)))))
       )
  |> List.map (fun (l,t,l') -> (l,t ~vars,l'))
  
let mk_transition_prob lhs (cost: Polynomial.t) (probability: float) rhs (formula: Formula.t) (vars:Var.t list): Transition.t list =
  formula
  |> Formula.constraints
  |> List.map (fun constr ->
	 (Location.of_string (Tuple2.first lhs),
          TransitionLabel.mk
            ~com_kind:(Tuple2.first rhs)
            ~targets:(Tuple2.second rhs)
            ~patterns:(List.map Var.of_string (Tuple2.second lhs))
            ~guard:constr 
            ~cost:cost
            ~probability:probability,
          (Location.of_string (Tuple2.first (List.hd (Tuple2.second rhs)))))
       )
  |> List.map (fun (l,t,l') -> (l,t ~vars,l'))

let default_vars =
  ["x"; "y"; "z"; "u"; "v"; "w"; "p"; "q"]
  |> List.map Var.of_string

let mk_transition_prob_simple (start: string) (cost: Polynomial.t) (probability: float) (rhs: string * (string * Polynomial.t list) list) (formula: Formula.t): Transition.t list =
  formula
  |> Formula.constraints
  |> List.map (fun constr ->
	 (Location.of_string start,
          TransitionLabel.mk
            ~probability:probability
            ~com_kind:(Tuple2.first rhs)
            ~targets:(Tuple2.second rhs)
            ~patterns:default_vars
            ~guard:constr 
            ~cost:cost
            ~vars:default_vars, (Location.of_string (Tuple2.first (List.hd (Tuple2.second rhs)))))
       )

let mk_transition_simple (start: string) (cost: Polynomial.t) (rhs: string * (string * Polynomial.t list) list) (formula: Formula.t): Transition.t list =
  formula
  |> Formula.constraints
  |> List.map (fun constr ->
	 (Location.of_string start,
          TransitionLabel.mk
            ~probability:1.0
            ~com_kind:(Tuple2.first rhs)
            ~targets:(Tuple2.second rhs)
            ~patterns:default_vars
            ~guard:constr 
            ~cost:cost
            ~vars:default_vars, (Location.of_string (Tuple2.first (List.hd (Tuple2.second rhs)))))
       )

       
let mk_program_simple (transitions: Transition.t list): Program.t =
  transitions
  |> List.hd
  |> Transition.src
  |> Program.from transitions

let mk_program goal start vars (transitions: Transition.t list): Program.t =
  Program.from transitions start
