open Batteries
open Program.Types

let logger = Logging.(get Size)

type kind = [ `Lower | `Upper ] [@@deriving show]

let compute_
      (kind: kind)
      (program: Program.t)
      (rvg: RVG.t)
      (get_lsb: RV.t -> LocalSizeBound.t)
      (get_timebound: Transition.t -> Bound.t)
      (get_sizebound: kind -> Transition.t -> Var.t -> Bound.t)
      (scc: RV.t list) =

  (** All transitions that are present in the scc. *)
  let transitions =
    scc
    |> List.map (fun (t,v) -> t)
    |> List.unique ~eq:Transition.equal
  in

  (** Returns all the variables with which the given transition does occur as result variable in the scc. *)
  let get_scc_vars transition =
    scc
    |> List.filter (fun (t,v) -> Transition.equal t transition)
    |> List.map (fun (t,v) -> v)
    |> List.enum
  in  
    
  (** Returns all the variables that may influence the given result variable and get changed in the scc. *)
  let scc_variables rv =
    rv
    |> RVG.pre rvg
    |> Util.intersection RV.equal (List.enum scc)
    |> Enum.map (fun (t,v) -> v)
  in
  
  (** The transition scaling factor corresponds to (s_t)^R(t) *)
  let transition_scaling_factor t =
    let execute () =

      (** Computes for each transition max(abs(pre(alpha)) intersected with C for all alpha in C_t) and multiplies the results. *)
      let extreme_affecting_scc_variables =
        t
        |> get_scc_vars
        |> Enum.map (fun v -> scc_variables (t,v))
        |> Enum.map Enum.count
        |> Util.max_option (>)
        |? 1
      in
      
      (** Computes for each transition max(s_alpha for all alpha in C_t) and multiplies the results. *)
      let extreme_scaling_factor =
        t
        |> get_scc_vars
        |> Enum.map (fun v -> get_lsb (t,v))
        |> Enum.map LocalSizeBound.factor
        |> Util.max_option (>)
        |? 1
      in
  
      Bound.exp
        (OurInt.of_int (extreme_scaling_factor * extreme_affecting_scc_variables))
        (get_timebound t)
      
    in Logger.with_log logger Logger.DEBUG
                       (fun () -> "transition scaling factor", ["transition", Transition.to_id_string t])
                       ~result:Bound.to_string
                       execute
  in
  
  (** The overall scaling factor is defined as the product over all scaling factors of transitions in the scc, 
      each scaling factor with the timebound of the transition as exponent.
      prod_{t in T} (s_t)^R(t) *)
  let overall_scaling_factor =
    transitions
    |> List.enum
    |> Enum.map transition_scaling_factor
    |> Bound.product
    |> tap (fun result -> Logger.log logger Logger.DEBUG (fun () -> "overall scaling factor", ["result", Bound.to_string result]))
  in
  
  let incoming_vars_effect (t,v) =
    let execute () =
      (t,v)
      |> get_lsb
      |> LocalSizeBound.vars
      |> VarSet.enum
      |> Util.without Var.equal (scc_variables (t,v))
      |> Enum.map (fun v ->
             (t,v)
             |> RVG.pre rvg
             |> Util.without RV.equal (List.enum scc)
             |> Enum.filter (fun (t,v') -> Var.equal v v')
             |> Enum.map (fun (t,v) -> get_sizebound kind t v)
             |> Bound.maximum
           )
      |> Bound.sum
    in Logger.with_log logger Logger.DEBUG
                       (fun () -> "incoming vars effect", ["alpha", RV.to_id_string (t,v)])
                       ~result:Bound.to_string
                       execute
  in
  
  (** The highest effect of the specific transition in the scc.
      A transition can manipulate different variables.
      The result of this function is for all those variables the highest incoming value. *)  
  let transition_effect t =
    let execute () =
      t
      |> get_scc_vars
      |> Enum.map (fun v ->
             Bound.(max zero (of_int (LocalSizeBound.constant (get_lsb (t,v))) + incoming_vars_effect (t,v)))
           )
      |> Bound.maximum
    in Logger.with_log logger Logger.DEBUG
                       (fun () -> "transition effect", ["transition", Transition.to_id_string t])
                       ~result:Bound.to_string
                       execute
  in
  
  let effects =
    transitions
    |> List.enum
    |> Enum.map (fun t -> Bound.(get_timebound t * transition_effect t))
    |> Bound.sum
    |> tap (fun result -> Logger.log logger Logger.DEBUG (fun () -> "effects", ["result", Bound.to_string result]))
  in
  
  let sign =
    match kind with
    | `Lower -> Bound.neg
    | `Upper -> identity
  in
              
  Bound.(sign (overall_scaling_factor * effects))

            
(** Computes a bound for a nontrivial scc. That is an scc which consists of a loop.
    Corresponds to 'SizeBounds for nontrivial SCCs'. *)
let compute kind program rvg get_timebound get_sizebound scc =
  let execute () =
    LocalSizeBound.sizebound_local_scc kind scc
    |> Option.map (fun get_lsb ->
           compute_ kind program rvg (get_lsb kind) get_timebound get_sizebound scc
         )
    |? match kind with
       | `Lower -> Bound.minus_infinity
       | `Upper -> Bound.infinity
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "compute nontrivial bound", ["kind", show_kind kind;
                                                             "scc", RVG.rvs_to_id_string scc])
                     ~result:Bound.to_string
                     execute
