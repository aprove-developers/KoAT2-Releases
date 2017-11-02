open Batteries

module TransitionSet = Set.Make(Program.Transition)
             
let logger = Logger.make_log "size"

type kind = [ `Lower | `Upper ]
           
(* Returns the maximum of all incoming sizebounds applicated to the local sizebound.
       Corresponds to 'SizeBounds for trivial SCCs':
       S'(alpha) = max{ S_l(alpha)(S(t',v_1),...,S(t',v_n)) | t' in pre(t) } *)
let incoming_bound (kind: kind)
                   (program: Program.t)
                   (appr: Approximation.t)
                   (local_sizebound: Bound.t)
                   (t: Program.Transition.t)
    : Bound.t =
  let execute () =
    let substitute_with_prevalues t' = Bound.substitute_f (Approximation.sizebound kind appr t') local_sizebound in
    t
    |> Program.pre program
    |> Enum.map substitute_with_prevalues
    |> match kind with
        | `Lower -> Bound.minimum
        | `Upper -> Bound.maximum
  in Logger.with_log logger Logger.DEBUG
                  (fun () -> "compute highest incoming bound", ["lsb", Bound.to_string local_sizebound; "transition", Program.Transition.to_string t])
                  ~result:Bound.to_string
                  execute

(* Improves a trivial scc. That is an scc which consists only of one result variable.
       Corresponds to 'SizeBounds for trivial SCCs'. *)
let improve_trivial_scc (kind: kind)
                        (program: Program.t)
                        (appr: Approximation.t)
                        (t,v)
    : Approximation.t =
  let execute () =
    let (local_sizebound: Bound.t) = LocalSizeBound.(as_bound (sizebound_local kind (Program.Transition.label t) v)) in
    let newbound =
      if Program.is_initial program t then
        local_sizebound
      else incoming_bound kind program appr local_sizebound t
    in Approximation.add_sizebound kind newbound t v appr
  in Logger.with_log logger Logger.DEBUG
                  (fun () -> "improve trivial scc", ["rv", Program.RV.to_string (t,v)])
                  execute

(* Computes for each transition max{s_alpha | alpha in C_t} and multiplies the results. *)
let extreme_scaling_factor (kind: kind)
                           (ct: Program.RV.t Enum.t)
    : int =
  ct
  |> Enum.map (LocalSizeBound.sizebound_local_rv kind)
  |> Enum.map Option.get (* Should exist *)
  |> Enum.map LocalSizeBound.abs_factor
  |> Util.max_option (>)
  |? 1

let scc_variables (rvg: Program.RVG.t)
                  (scc: Program.RVG.scc)
                  (rv: Program.RV.t)
    : Var.t Enum.t =
  Program.RVG.pre rvg rv
  |> Util.intersection Program.RV.equal scc
  |> Enum.map (fun (t,v) -> v)
  
(* Computes for each transition max{ |pre(alpha)| intersected with C | alpha in C_t } and multiplies the results. *)
let extreme_affecting_scc_variables (kind: kind) (* TODO Relevant for only positive and only negative effects. *)
                                    (rvg: Program.RVG.t)
                                    (scc: Program.RVG.scc)
                                    (ct: Program.RV.t Enum.t)
    : int =
  ct
  |> Enum.map (scc_variables rvg scc)
  (* Filter also all pre variables that can only have a negative effect. *)
  |> Enum.map Enum.count
  |> Util.max_option (>)
  |? 1

let transition_scaling_factor (kind: kind)
                              (rvg: Program.RVG.t)
                              (appr: Approximation.t)
                              (scc: Program.RVG.scc)
                              (ct: Program.RV.t Enum.t)
    : Bound.t =
  let (transition, _) = Option.get (Enum.peek ct) (* We require ct to be non-empty *) in
  Bound.exp (OurInt.of_int (extreme_scaling_factor kind ct *
                              extreme_affecting_scc_variables kind rvg scc (Enum.clone ct)))
            (Approximation.timebound appr transition) 
  

let overall_scaling_factor (kind: kind)
                           (rvg: Program.RVG.t)
                           (appr: Approximation.t)
                           (scc: Program.RVG.scc)
    : Bound.t =
  scc
  |> Enum.clone
  |> Enum.group_by (fun (t1, v1) (t2, v2) -> Program.Transition.equal t1 t2)
  |> Enum.map (transition_scaling_factor kind rvg appr (Enum.clone scc))
  |> Bound.product

let incoming_vars_effect (kind: kind)
                         (rvg: Program.RVG.t)
                         (appr: Approximation.t)
                         (scc: Program.RVG.scc)
                         (vars: VarSet.t)
                         (transition: Program.Transition.t)
                         (alpha: Program.RV.t)
    : Bound.t =
  vars
  |> VarSet.enum
  (* TODO Performance issue *)
  |> Enum.filter (fun v -> not (Enum.exists (Var.equal v) (scc_variables rvg (Enum.clone scc) alpha)))
  |> Enum.map (fun v ->
         Program.RVG.pre rvg alpha 
         |> Enum.filter (fun rv -> not (Enum.exists (Program.RV.equal rv) (Enum.clone scc)))
         |> Enum.filter (fun (t,v') -> Var.equal v v')
         |> Enum.map (fun (t,v) -> Approximation.sizebound kind appr t v)
         |> Bound.maximum
       )
  |> Bound.sum
  
let transition_effect (kind: kind)
                      (rvg: Program.RVG.t)
                      (appr: Approximation.t)
                      (scc: Program.RVG.scc)
                      (ct: Program.RV.t Enum.t)
                      (transition: Program.Transition.t)
    : Bound.t =
  ct
  |> Enum.map (fun alpha -> (alpha, Option.get (LocalSizeBound.sizebound_local_rv kind alpha)))
  (* Should exist *)
  |> Enum.map (fun (alpha, lsb) ->
         Bound.(max zero (of_int lsb.LocalSizeBound.constant + incoming_vars_effect kind rvg appr (Enum.clone scc) lsb.LocalSizeBound.vars transition alpha))
       )
  |> Bound.maximum

let effects (kind: kind)
            (rvg: Program.RVG.t)
            (appr: Approximation.t)
            (scc: Program.RVG.scc)
    : Bound.t =
  scc
  |> Enum.clone
  |> Enum.group_by (fun (t1, v1) (t2, v2) -> Program.Transition.equal t1 t2)
  |> Enum.map (fun ct ->
         let (transition, _) = Option.get (Enum.peek ct) (* We require ct to be non-empty *) in
         Bound.(Approximation.timebound appr transition * transition_effect kind rvg appr (Enum.clone scc) ct transition)
       )
  |> Bound.sum

let sign = function
  | `Lower -> Bound.neg Bound.one
  | `Upper -> Bound.one
  
(* Improves a nontrivial scc. That is an scc which consists of more than one result variable.
       Corresponds to 'SizeBounds for nontrivial SCCs'. *)
let improve_nontrivial_scc (kind: kind)
                           (program: Program.t)
                           (rvg: Program.RVG.t)
                           (appr: Approximation.t)
                           (scc: Program.RVG.scc)
    : Approximation.t =
  let execute () =
    (* Only if all lsbs have a bound of our required form *)
    if scc
       |> Enum.clone
       |> Enum.map (LocalSizeBound.sizebound_local_rv kind)
       |> Enum.for_all Option.is_some
    then
      let new_bound = Bound.(sign kind * overall_scaling_factor kind rvg appr (Enum.clone scc) * effects kind rvg appr (Enum.clone scc)) in
      Approximation.add_sizebounds kind new_bound (Enum.clone scc) appr
    else appr
  in Logger.with_log logger Logger.DEBUG
                  (fun () -> "improve nontrivial scc", ["scc", String.concat "," (List.of_enum (Enum.map Program.RV.to_string (Enum.clone scc)))])
                  execute
         
(* Improves a whole scc. *)
let improve_scc (program: Program.t)
                (rvg: Program.RVG.t)
                (appr: Approximation.t)
                (scc: Program.RV.t list)
    : Approximation.t  =
  match scc with
  | [((l,t,l'),v)] when not (Program.Location.equal l l') ->
     appr
     |> fun appr -> improve_trivial_scc `Upper program appr ((l,t,l'),v)
     |> fun appr -> improve_trivial_scc `Lower program appr ((l,t,l'),v)         
  | scc ->
     appr
     |> fun appr -> improve_nontrivial_scc `Upper program rvg appr (List.enum scc)
     |> fun appr -> improve_nontrivial_scc `Lower program rvg appr (List.enum scc)
         
let improve program appr =
  let execute () =
    let module C = Graph.Components.Make(Program.RVG) in
    let rvg = Program.rvg program in
    List.fold_left (fun appr scc -> improve_scc program rvg appr scc) appr (List.rev (C.scc_list rvg))
  in Logger.with_log logger Logger.DEBUG
                  (fun () -> "improve size bounds", [])
                  execute
