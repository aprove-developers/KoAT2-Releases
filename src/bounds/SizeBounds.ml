open Batteries
open Program.Types
   
module TransitionSet = Set.Make(Transition)
             
let logger = Logger.make_log "size"

type kind = [ `Lower | `Upper ] [@@deriving show]
           
let incoming_bound kind program get_sizebound local_sizebound t =
  let execute () =
    let substitute_with_prevalues t' = Bound.substitute_f (get_sizebound kind t') local_sizebound in
    t
    |> Program.pre program
    |> Enum.map substitute_with_prevalues
    |> match kind with
        | `Lower -> Bound.minimum
        | `Upper -> Bound.maximum
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "compute highest incoming bound", ["lsb", Bound.to_string local_sizebound;
                                                                   "transition", Transition.to_id_string t])
                  ~result:Bound.to_string
                  execute

let compute_trivial_bound kind program get_sizebound (t,v) =
  let execute () =
    let (lsb: Bound.t) = LocalSizeBound.(as_bound (sizebound_local kind (Transition.label t) v)) in
    if Program.is_initial program t then
      lsb
    else incoming_bound kind program get_sizebound lsb t
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "compute trivial bound", ["kind", show_kind kind;
                                                          "rv", RV.to_id_string (t,v)])
                     ~result:Bound.to_string
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

let transition_scaling_factor kind rvg get_timebound scc ct =
  let execute () =
    let (transition, _) = List.hd ct (* We require ct to be non-empty *) in
    Bound.exp (OurInt.of_int (extreme_scaling_factor kind ct *
                                extreme_affecting_scc_variables kind rvg scc ct))
              (get_timebound transition) 
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "transition scaling factor", ["ct", RVG.rvs_to_id_string ct; "scc", RVG.rvs_to_id_string scc])
                     ~result:Bound.to_string
                     execute
  
let overall_scaling_factor kind rvg get_timebound scc=
  let execute () =
    scc
    |> List.enum
    |> Enum.group_by (fun (t1, v1) (t2, v2) -> Transition.equal t1 t2)
    |> Enum.map List.of_enum
    |> Enum.map (transition_scaling_factor kind rvg get_timebound scc)
    |> Bound.product
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "overall scaling factor", ["scc", RVG.rvs_to_id_string scc])
                     ~result:Bound.to_string
                     execute


let incoming_vars_effect kind rvg get_sizebound scc vars transition alpha =
  let execute () =
    vars
    |> VarSet.enum
    |> Util.without Var.equal (scc_variables rvg scc alpha)
    |> Enum.map (fun v ->
           RVG.pre rvg alpha
           |> Util.without RV.equal (List.enum scc)
           |> Enum.filter (fun (t,v') -> Var.equal v v')
           |> Enum.map (fun (t,v) -> get_sizebound kind t v)
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

let transition_effect kind rvg get_sizebound scc ct transition =
  let execute () =
    ct
    |> List.enum
    |> Enum.map (fun alpha -> (alpha, Option.get (LocalSizeBound.sizebound_local_rv kind alpha)))
    (* Should exist *)
    |> Enum.map (fun (alpha, lsb) ->
           Bound.(max zero (of_int lsb.LocalSizeBound.constant + incoming_vars_effect kind rvg get_sizebound scc lsb.LocalSizeBound.vars transition alpha))
         )
    |> Bound.maximum
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "transition effect", ["scc", RVG.rvs_to_id_string scc;
                                                      "ct", RVG.rvs_to_id_string ct;
                                                      "transition", Transition.to_id_string transition])
                     ~result:Bound.to_string
                     execute

let effects kind rvg get_timebound get_sizebound scc =
  let execute () =
    scc
    |> List.enum
    |> Enum.group_by (fun (t1, v1) (t2, v2) -> Transition.equal t1 t2)
    |> Enum.map List.of_enum
    |> Enum.map (fun ct ->
           let ((transition, v) :: _) = ct (* We require ct to be non-empty *) in
           Bound.(get_timebound transition * transition_effect kind rvg get_sizebound scc ct transition)
         )
    |> Bound.sum
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "effects", ["scc", RVG.rvs_to_id_string scc])
                     ~result:Bound.to_string
                     execute


let sign = function
  | `Lower -> Bound.neg Bound.one
  | `Upper -> Bound.one
  
let compute_nontrivial_bound kind program rvg get_timebound get_sizebound scc =
  let execute () =
    (* Only if all lsbs have a bound of our required form *)
    if scc
       |> List.enum
       |> Enum.map (LocalSizeBound.sizebound_local_rv kind)
       |> Enum.for_all Option.is_some
    then
      Bound.(sign kind
             * overall_scaling_factor kind rvg get_timebound scc
             * effects kind rvg get_timebound get_sizebound scc)
    else
      match kind with
      | `Lower -> Bound.minus_infinity
      | `Upper -> Bound.infinity
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "compute nontrivial bound", ["kind", show_kind kind;
                                                             "scc", RVG.rvs_to_id_string scc])
                     ~result:Bound.to_string
                     execute
    
let improve_scc program rvg appr = function
  | [((l,t,l'),v)] when not (Location.equal l l') ->
     let add_trivial_bound kind =
       let new_bound = compute_trivial_bound kind program (fun kind -> Approximation.sizebound kind appr) ((l,t,l'),v) in
       Approximation.add_sizebound kind new_bound (l,t,l') v appr
     in appr
     |> fun appr -> add_trivial_bound `Upper
     |> fun appr -> add_trivial_bound `Lower
  | scc ->
     let add_nontrivial_bound kind =
       let new_bound = compute_nontrivial_bound kind program rvg (Approximation.timebound appr) (fun kind -> Approximation.sizebound kind appr) scc in
       Approximation.add_sizebounds kind new_bound scc appr
     in appr
     |> fun appr -> add_nontrivial_bound `Upper
     |> fun appr -> add_nontrivial_bound `Lower
         
let improve program appr =
  let execute () =
    let module C = Graph.Components.Make(RVG) in
    let rvg = Program.rvg program in
    List.fold_left (fun appr scc -> improve_scc program rvg appr scc) appr (List.rev (C.scc_list rvg))
  in Logger.with_log logger Logger.DEBUG
                  (fun () -> "improve size bounds", [])
                  execute
