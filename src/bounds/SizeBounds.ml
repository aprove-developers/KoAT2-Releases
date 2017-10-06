open Batteries

module TransitionSet = Set.Make(Program.Transition)
             
(* Returns the maximum of all incoming sizebounds applicated to the local sizebound.
       Corresponds to 'SizeBounds for trivial SCCs':
       S'(alpha) = max{ S_l(alpha)(S(t',v_1),...,S(t',v_n)) | t' in pre(t) } *)
let highest_incoming_bound (program: Program.t)
                           (appr: Approximation.t)
                           (local_sizebound: Bound.t)
                           (t: Program.Transition.t)
    : Bound.t =
  let substitute_with_prevalues t' = Bound.substitute_f Approximation.(sizebound Upper appr t') local_sizebound in
     t
  |> Program.pre program
  |> TransitionSet.to_list
  |> List.map substitute_with_prevalues
  |> Bound.maximum

(* Improves a trivial scc. That is an scc which consists only of one result variable.
       Corresponds to 'SizeBounds for trivial SCCs'. *)
let improve_trivial_scc (program: Program.t)
                        (appr: Approximation.t)
                        (t,v)
    : Approximation.t =
  let (local_sizebound: Bound.t) = TransitionLabel.(LocalSizeBound.(as_bound (sizebound_local Upper (Program.Transition.label t) v))) in
  let newbound =
    if Program.is_initial program t then
      local_sizebound
    else highest_incoming_bound program appr local_sizebound t
  in Approximation.(add_sizebound Upper newbound t v appr)      

(* Returns all bounds for result variables entering the scc. *)
let incoming_bounds (rvg: Program.RVG.t)
                    (appr: Approximation.t)
                    (scc: Program.RV.t list)
  : Bound.t Enum.t =
     Program.RVG.entry_points rvg scc
  |> Enum.map (fun (t,v) -> Approximation.(sizebound Upper appr t v))

(* Returns all constants which bound a result variable of the scc of an rvg. *)
let constants (scc: Program.RV.t list)
    : Bound.t Enum.t =
     scc
  |> List.enum
  |> Enum.map (LocalSizeBound.sizebound_local_rv TransitionLabel.Upper)
  |> Enum.filter_map LocalSizeBound.equality_constant
  |> Enum.map Bound.of_int

(* Returns the highest possible start value for the nontrivial scc approximation.
   This is the maximum of the highest values of variables which reach the scc and the constants assigned in the scc. *)
let highest_start_value (rvg: Program.RVG.t)
                        (appr: Approximation.t)
                        (scc: Program.RV.t list)
    : Bound.t =
     Enum.append (incoming_bounds rvg appr scc) (constants scc)
  |> List.of_enum
  |> Bound.maximum 

(* Computes the maximum of the enum if non-empty, else returns None. *)
let max (enum: int Enum.t): int Option.t =
  let f = function
    | (None, y) -> Some y
    | (Some x, y) -> if x > y then Some x else Some y in
  Enum.fold (curry f) None enum

(* Returns the highest adding constant of the transition for any variable in the scc if one exists, else returns None. *)
let max_adding_constant (scc: Program.RVG.scc)
                        (t: Program.Transition.t)
    : int Option.t =
  scc
  |> List.enum
  |> Enum.filter (fun (t',v) -> Program.Transition.equal t t')
  |> Enum.map (LocalSizeBound.sizebound_local_rv TransitionLabel.Upper)
  |> Enum.filter_map LocalSizeBound.addsconstant_constant
  |> max

(* Computes the effect of a single transition which possibly adds a constant to the value of any variable in the SCC. *)
let single_adding_constants_effect (scc: Program.RVG.scc)
                                   (appr: Approximation.t)
                                   (t: Program.Transition.t)
    : Bound.t Option.t =
  max_adding_constant scc t
  |> Option.map (fun max_d ->
         let timebound_kind = Approximation.(if max_d >= 0 then Upper else Lower) in
         Bound.(Approximation.(timebound timebound_kind appr t) * of_int max_d)
       )

(* Computes the effect of all transitions which add constants to the value of any variables in the SCC. *)
let adding_constants_effect (rvg: Program.RVG.t)
                            (appr: Approximation.t)
                            (scc: Program.RVG.scc)
    : Bound.t =
  Program.RVG.transitions scc
  |> Enum.filter_map (single_adding_constants_effect scc appr)
  |> Enum.fold Bound.add Bound.zero

(* Improves a nontrivial scc. That is an scc which consists of more than one result variable.
       Corresponds to 'SizeBounds for nontrivial SCCs'. *)
let improve_nontrivial_scc (program: Program.t)
                           (rvg: Program.RVG.t)
                           (appr: Approximation.t)
                           (scc: Program.RV.t list)
    : Approximation.t =
  let lsbs =
    List.map (LocalSizeBound.sizebound_local_rv TransitionLabel.Upper) scc
  in
  if List.for_all (fun lsb -> LocalSizeBound.boundtype lsb != `Unbound && LocalSizeBound.boundtype lsb != `ScaledSum) lsbs then
    let new_bound =
      Bound.(highest_start_value rvg appr scc
             + adding_constants_effect rvg appr scc)
    in
    Approximation.(add_sizebounds Upper new_bound scc appr) 
  else
    appr
  

(* Improves a whole scc. *)
let improve_scc (program: Program.t)
                (rvg: Program.RVG.t)
                (appr: Approximation.t)
                (scc: Program.RV.t list)
    : Approximation.t  =
  match scc with
  | [rv] -> improve_trivial_scc program appr rv
  | scc -> improve_nontrivial_scc program rvg appr scc
         
let improve program appr =
  let module C = Graph.Components.Make(Program.RVG) in
  let rvg = Program.rvg program in
  List.fold_left (fun appr scc -> improve_scc program rvg appr scc) appr (C.scc_list rvg)
