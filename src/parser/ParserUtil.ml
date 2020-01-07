open Batteries
open Formulas
open Polynomials
open BoundsInst
open ProgramTypes

let mk_transition trans_id_counter lhs (cost: Polynomial.t) gtcost (rhs: string * ((string * (TransitionLabel.UpdateElement.t list)) list)) (formula: Formula.t) (vars:Var.t list): Transition.t list =
  formula
  |> Formula.constraints
  |> List.map (fun constr ->
	 (Location.of_string (Tuple2.first lhs),
          TransitionLabel.mk
            trans_id_counter
            ~cvect:(cost, gtcost |> RealPolynomial.of_intpoly |> RealBound.of_poly)
            ~com_kind:(Tuple2.first rhs)
            ~targets:(Tuple2.second rhs)
            ~patterns:(List.map Var.of_string (Tuple2.second lhs))
            ~guard:constr,
          (Location.of_string (Tuple2.first (List.hd (Tuple2.second rhs)))))
       )
  |> List.map (fun (l,t,l') -> (l,t ~vars,l'))

  (*So far recursion cannot be parsed, therefore the location is taken as the head of the list as non singleton lists yield an exception*)
  let mk_transition_prob trans_id_counter lhs (cost: Polynomial.t) gtcost (rhs: (OurFloat.t * string * ((string * (TransitionLabel.UpdateElement.t list)) list)) list) (formula: Formula.t) (vars:Var.t list): Transition.t list =
  formula
  |> Formula.constraints
  |> List.map (fun constr ->
          let id = TransitionLabel.get_unique_gt_id trans_id_counter () in
          List.map (fun (prob, comkind, targets) ->
            (Location.of_string (Tuple2.first lhs),
              TransitionLabel.mk_prob
                trans_id_counter
                ~cvect:(cost, gtcost |> RealPolynomial.of_intpoly |> RealBound.of_poly)
                ~com_kind:comkind
                ~targets:targets
                ~patterns:(List.map Var.of_string (Tuple2.second lhs))
                ~guard:constr
                ~gt_id:id
                ~probability:prob,
              (Location.of_string (Tuple2.first (List.hd targets))))
        ) rhs)
  |> List.concat
  |> List.map (fun (l,t,l') -> (l,t ~vars,l'))

let cartesian l l' =
  List.concat (List.map (fun e -> List.map (fun e' -> (e,e')) l') l)

let default_vars =
  ["x"; "y"; "z"; "u"; "v"; "w"; "p"; "q"]
  |> List.map Var.of_string

let mk_transition_simple trans_id_counter (start: string) (cost: Polynomial.t) gtcost (rhs: string * (string * TransitionLabel.UpdateElement.t list) list) (formula: Formula.t): Transition.t list =
  formula
  |> Formula.constraints
  |> List.map (fun constr ->
	 (Location.of_string start,
          TransitionLabel.mk
            trans_id_counter
            ~com_kind:(Tuple2.first rhs)
            ~targets:(Tuple2.second rhs)
            ~patterns:default_vars
            ~guard:constr
            ~cvect:(cost, RealPolynomial.of_intpoly gtcost |> RealBound.of_poly)
            ~vars:default_vars, (Location.of_string (Tuple2.first (List.hd (Tuple2.second rhs)))))
       )

let mk_transition_simple_prob trans_id_counter (start: string) (cost: Polynomial.t) gtcost (rhs: (OurFloat.t * string * (string * TransitionLabel.UpdateElement.t list) list) list) (formula: Formula.t): Transition.t list =
  formula
  |> Formula.constraints
  |> List.map (fun constr ->
          List.map (fun (prob, comkind, targets) ->
          let id = TransitionLabel.get_unique_gt_id trans_id_counter () in
          (Location.of_string start,
            TransitionLabel.mk_prob
              trans_id_counter
              ~cvect:(cost, RealPolynomial.of_intpoly gtcost |> RealBound.of_poly)
              ~com_kind:comkind
              ~targets:targets
              ~patterns:default_vars
              ~guard:constr
              ~vars:default_vars
              ~gt_id:id
              ~probability:prob,
            (Location.of_string (Tuple2.first (List.hd targets))))
       ) rhs)
  |> List.concat


let mk_program_simple (transitions: Transition.t list): Program.t =
  transitions
  |> List.hd
  |> Transition.src
  |> Program.from transitions

let mk_program goal start vars (transitions: Transition.t list): Program.t =
  Program.from transitions start

let ourfloat_of_decimal_string (str: string): OurFloat.t =
  let str_before_point = String.split str ~by:(".") |> Tuple2.first in
  let str_after_point = String.split str ~by:(".") |> Tuple2.second in
  let numerator =
    if str_after_point = "" then OurFloat.zero else OurFloat.of_string str_after_point
  in
  let denominator =
    OurFloat.pow (OurFloat.of_int 10) (String.length str_after_point)
  in
  let fractional = if str_after_point = "" then OurFloat.zero else OurFloat.( numerator/denominator ) in
  let leading = if str_before_point = "" then OurFloat.zero else OurFloat.of_string str_before_point in
  OurFloat.(leading + fractional)
