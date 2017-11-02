open Batteries
open Program.Types
   
module TransitionSet = Set.Make(Transition)
             
let logger = Logger.make_log "size"

type kind = [ `Lower | `Upper ] [@@deriving show]
           
let incoming_bound kind program appr local_sizebound t =
  let execute () =
    let substitute_with_prevalues t' = Bound.substitute_f (Approximation.sizebound kind appr t') local_sizebound in
    t
    |> Program.pre program
    |> Enum.map substitute_with_prevalues
    |> match kind with
        | `Lower -> Bound.minimum
        | `Upper -> Bound.maximum
  in Logger.with_log logger Logger.DEBUG
                  (fun () -> "compute highest incoming bound", ["lsb", Bound.to_string local_sizebound; "transition", Transition.to_id_string t])
                  ~result:Bound.to_string
                  execute

let compute_trivial_bound kind program appr lsb t =
  let execute () =
    if Program.is_initial program t then
      lsb
    else incoming_bound kind program appr lsb t
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "compute trivial bound", ["lsb", Bound.to_string lsb;
                                                          "transition", Transition.to_id_string t])
                     ~result:Bound.to_string
                     execute

let improve_trivial_scc kind program appr (t,v) =
  let execute () =
    let (local_sizebound: Bound.t) = LocalSizeBound.(as_bound (sizebound_local kind (Transition.label t) v)) in
    let new_bound = compute_trivial_bound kind program appr local_sizebound t in
    Approximation.add_sizebound kind new_bound t v appr
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "improve trivial scc", ["kind", show_kind kind;
                                                        "rv", RV.to_id_string (t,v)])
                  execute

let extreme_scaling_factor kind ct =
  let execute () =
    ct
    |> List.enum
    |> Enum.map (LocalSizeBound.sizebound_local_rv kind)
    |> Enum.map Option.get (* Should exist *)
    |> Enum.map LocalSizeBound.abs_factor
    |> Util.max_option (>)
    |? 1
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "extreme scaling factor", ["ct", RVG.rvs_to_id_string ct])
                     ~result:Int.to_string
                     execute

let scc_variables rvg scc rv =
  RVG.pre rvg rv
  |> Util.intersection RV.equal (List.enum scc)
  |> Enum.map (fun (t,v) -> v)
  
let extreme_affecting_scc_variables kind rvg scc ct =
  let execute () =
    ct
    |> List.enum
    |> Enum.map (scc_variables rvg scc)
    (* Filter also all pre variables that can only have a negative effect. *)
    |> Enum.map Enum.count
    |> Util.max_option (>)
    |? 1
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "extreme affecting scc variables", ["scc", RVG.rvs_to_id_string scc])
                     ~result:Int.to_string
                     execute

let transition_scaling_factor kind rvg appr scc ct =
  let execute () =
    let (transition, _) = List.hd ct (* We require ct to be non-empty *) in
    Bound.exp (OurInt.of_int (extreme_scaling_factor kind ct *
                                extreme_affecting_scc_variables kind rvg scc ct))
              (Approximation.timebound appr transition) 
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "transition scaling factor", ["ct", RVG.rvs_to_id_string ct; "scc", RVG.rvs_to_id_string scc])
                     ~result:Bound.to_string
                     execute
  
let overall_scaling_factor kind rvg appr scc=
  let execute () =
    scc
    |> List.enum
    |> Enum.group_by (fun (t1, v1) (t2, v2) -> Transition.equal t1 t2)
    |> Enum.map List.of_enum
    |> Enum.map (transition_scaling_factor kind rvg appr scc)
    |> Bound.product
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "overall scaling factor", ["scc", RVG.rvs_to_id_string scc])
                     ~result:Bound.to_string
                     execute


let incoming_vars_effect kind rvg appr scc vars transition alpha =
  let execute () =
    vars
    |> VarSet.enum
    |> Util.without Var.equal (scc_variables rvg scc alpha)
    |> Enum.map (fun v ->
           RVG.pre rvg alpha
           |> Util.without RV.equal (List.enum scc)
           |> Enum.filter (fun (t,v') -> Var.equal v v')
           |> Enum.map (fun (t,v) -> Approximation.sizebound kind appr t v)
           |> Bound.maximum
         )
    |> Bound.sum
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "incoming vars effect", ["scc", RVG.rvs_to_id_string scc;
                                                         "vars", VarSet.to_string vars;
                                                         "transition", Transition.to_id_string transition;
                                                         "alpha", RV.to_id_string alpha])
                     ~result:Bound.to_string
                     execute

let transition_effect kind rvg appr scc ct transition=
  let execute () =
    ct
    |> Enum.map (fun alpha -> (alpha, Option.get (LocalSizeBound.sizebound_local_rv kind alpha)))
    (* Should exist *)
    |> Enum.map (fun (alpha, lsb) ->
           Bound.(max zero (of_int lsb.LocalSizeBound.constant + incoming_vars_effect kind rvg appr scc lsb.LocalSizeBound.vars transition alpha))
         )
    |> Bound.maximum
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "transition effect", ["scc", RVG.rvs_to_id_string scc;
                                                      "ct", RVG.rvs_to_id_string (List.of_enum ct);
                                                      "transition", Transition.to_id_string transition])
                     ~result:Bound.to_string
                     execute

let effects kind rvg appr scc =
  let execute () =
    scc
    |> List.enum
    |> Enum.group_by (fun (t1, v1) (t2, v2) -> Transition.equal t1 t2)
    |> Enum.map (fun ct ->
           let (transition, _) = Option.get (Enum.peek ct) (* We require ct to be non-empty *) in
           Bound.(Approximation.timebound appr transition * transition_effect kind rvg appr scc ct transition)
         )
    |> Bound.sum
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "effects", ["scc", RVG.rvs_to_id_string scc])
                     ~result:Bound.to_string
                     execute


let sign = function
  | `Lower -> Bound.neg Bound.one
  | `Upper -> Bound.one
  
(* Improves a nontrivial scc. That is an scc which consists of more than one result variable.
       Corresponds to 'SizeBounds for nontrivial SCCs'. *)
let improve_nontrivial_scc kind program rvg appr scc =
  let execute () =
    (* Only if all lsbs have a bound of our required form *)
    if scc
       |> List.enum
       |> Enum.map (LocalSizeBound.sizebound_local_rv kind)
       |> Enum.for_all Option.is_some
    then
      let new_bound = Bound.(sign kind * overall_scaling_factor kind rvg appr scc * effects kind rvg appr scc) in
      Approximation.add_sizebounds kind new_bound scc appr
    else appr
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "improve nontrivial scc", ["kind", show_kind kind;
                                                           "scc", RVG.rvs_to_id_string scc])
                     execute
         
(* Improves a whole scc. *)
let improve_scc program rvg appr scc =
  match scc with
  | [((l,t,l'),v)] when not (Location.equal l l') ->
     appr
     |> fun appr -> improve_trivial_scc `Upper program appr ((l,t,l'),v)
     |> fun appr -> improve_trivial_scc `Lower program appr ((l,t,l'),v)         
  | scc ->
     appr
     |> fun appr -> improve_nontrivial_scc `Upper program rvg appr scc
     |> fun appr -> improve_nontrivial_scc `Lower program rvg appr scc
         
let improve program appr =
  let execute () =
    let module C = Graph.Components.Make(RVG) in
    let rvg = Program.rvg program in
    List.fold_left (fun appr scc -> improve_scc program rvg appr scc) appr (List.rev (C.scc_list rvg))
  in Logger.with_log logger Logger.DEBUG
                  (fun () -> "improve size bounds", [])
                  execute
