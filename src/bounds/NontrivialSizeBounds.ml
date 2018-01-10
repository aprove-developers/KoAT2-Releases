open Batteries
open Program.Types

let logger = Logging.(get Size)

type kind = [ `Lower | `Upper ] [@@deriving show]
type sign = [ `Pos | `Neg ] [@@deriving show]

let sign = function
  | `Lower -> Bound.neg
  | `Upper -> identity

let compute_
      (kind: kind)
      (program: Program.t)
      (rvg: RVG.t)
      (get_lsb: RV.t -> LocalSizeBound.t)
      (get_timebound: Transition.t -> Bound.t)
      (get_sizebound: kind -> Transition.t -> Var.t -> Bound.t)
      (scc: RV.t list) =

  (** All transitions that are present in the scc.
      Corresponds to T_C in the thesis. *)
  let transitions =
    scc
    |> List.map (fun (t,v) -> t)
    |> List.unique ~eq:Transition.same
    |> tap (fun transitions -> Logger.log logger Logger.DEBUG (fun () -> "transitions", ["result", Util.enum_to_string Transition.to_id_string (List.enum transitions)]))
  in

  (** Returns all the variables with which the given transition does occur as result variable in the scc. *)
  let get_scc_vars transition =
    scc
    |> List.filter (fun (t,v) -> Transition.same t transition)
    |> List.map (fun (t,v) -> v)
    |> List.unique ~eq:Var.equal
    |> tap (fun scc_vars -> Logger.log logger Logger.DEBUG (fun () -> "scc_vars", ["result", Util.enum_to_string Var.to_string (List.enum scc_vars)]))
    |> List.enum
  in  
    
  (** Returns all result variables that may influence the given result variable and that are part of the scc. *)
  let pre_in_scc rv =
    rv
    |> RVG.pre rvg
    |> Util.intersection RV.same (List.enum scc)
  in

  (** Returns all result variables that may influence the given result variable and that are not part of the scc. *)
  let pre_out_scc rv =
    rv
    |> RVG.pre rvg
    |> Util.without RV.same (List.enum scc)
  in

  (** Returns all result variables that may influence the given result variable from within the scc. 
      Corresponds to V_rv in the thesis. *)
  let scc_variables rv =
    rv
    |> pre_in_scc
    |> Enum.map (fun (t,v) -> v)
    |> Enum.uniq_by Var.equal
  in
    
  (** Corresponds to the definition of the transition scaling factor in the thesis. *)
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
        |> tap (fun result -> Logger.log logger Logger.DEBUG (fun () -> "extreme_affecting_scc_variables", ["result", Int.to_string result]))
      in
      
      (** Computes for each transition max(s_alpha for all alpha in C_t) and multiplies the results. *)
      let extreme_scaling_factor =
        t
        |> get_scc_vars
        |> Enum.map (fun v -> get_lsb (t,v))
        |> Enum.map LocalSizeBound.factor
        |> Util.max_option (>)
        |? 1
        |> tap (fun result -> Logger.log logger Logger.DEBUG (fun () -> "extreme_scaling_factor", ["result", Int.to_string result]))
      in
  
      OurInt.of_int (extreme_scaling_factor * extreme_affecting_scc_variables)
      
    in Logger.with_log logger Logger.DEBUG
                       (fun () -> "transition scaling factor", ["transition", Transition.to_id_string t])
                       ~result:OurInt.to_string
                       execute
  in
  
  (** Corresponds to the definition of the loop scaling factor in the thesis. *)
  let loop_scaling_factor =
    let execute () =
      transitions
      |> List.enum
      |> Enum.map (fun t -> Bound.exp (transition_scaling_factor t) (get_timebound t))
      |> Bound.product
    in Logger.with_log logger Logger.DEBUG
                       (fun () -> "loop_scaling_factor", [])
                       ~result:Bound.to_string
                       execute
  in

  (** Corresponds to the definition of the incoming constant in the thesis. *)
  let incoming_constant prekind rv v =
    let execute () =
      rv
      |> pre_out_scc
      |> Enum.filter (fun (_,v') -> Var.equal v v')
      |> Enum.map (fun (t,v) -> get_sizebound prekind t v)
      |> Enum.map (sign prekind)
      |> Bound.maximum
    in Logger.with_log logger Logger.DEBUG
                       (fun () -> "incoming_constant", ["prekind", show_kind kind;
                                                        "rv", RV.to_id_string rv;
                                                        "variable", Var.to_string v])
                       ~result:Bound.to_string
                       execute
  in
  
  (** Returns the constant of the local sizebound of the given result variable in a positive way. *)
  let rv_constant (t,v) =
    let execute () =
      (t,v)
      |> get_lsb
      |> LocalSizeBound.constant
      |> Bound.of_int
      |> sign kind
    in Logger.with_log logger Logger.DEBUG
                       (fun () -> "rv_constant", ["variable", RV.to_id_string (t,v)])
                       ~result:Bound.to_string
                       execute
  in
  
  (** Corresponds to the definition of the result variable effect in the thesis. *)
  let result_variable_effect rv =
    let affecting_vars sign =
      rv
      |> get_lsb
      |> LocalSizeBound.vars_of_sign sign
      |> VarSet.enum
      |> Util.without Var.equal (scc_variables rv)
    in
    let execute () =
      Bound.(
        max zero (rv_constant rv)
        + (affecting_vars `Pos
           |> Enum.map (incoming_constant (LocalSizeBound.pre_kind (kind, `Pos)) rv)
           |> Enum.map (max zero)
           |> sum
          )
        + (affecting_vars `Neg
           |> Enum.map (incoming_constant (LocalSizeBound.pre_kind (kind, `Neg)) rv)
           |> Enum.map (max zero)
           |> sum
          )
      )
    in Logger.with_log logger Logger.DEBUG
                       (fun () -> "result_variable_effect", ["rv", RV.to_id_string rv])
                       ~result:Bound.to_string
                       execute
  in

  (** Corresponds to the definition of the transition effect in the thesis. *)
  let transition_effect t =
    let execute () =
      t
      |> get_scc_vars
      |> Enum.map (fun v -> (t,v))
      |> Enum.map result_variable_effect
      |> Bound.maximum
    in Logger.with_log logger Logger.DEBUG
                       (fun () -> "transition_effect", ["transition", Transition.to_id_string t])
                       ~result:Bound.to_string
                       execute
  in

  (** Corresponds to the definition of the loop effect in the thesis. *)
  let loop_effect =
    let execute () =
      transitions
      |> List.enum
      |> Enum.map (fun t -> Bound.(get_timebound t * transition_effect t))
      |> Bound.sum
    in Logger.with_log logger Logger.DEBUG
                       (fun () -> "loop_effect", [])
                       ~result:Bound.to_string
                       execute
  in

  (** Corresponds to the definition of the starting value in the thesis. *)
  let starting_value kind =
    scc
    |> List.enum
    |> Enum.map pre_out_scc
    |> Enum.flatten
    |> Enum.map (uncurry (get_sizebound kind))
    |> Enum.map (sign kind)
    |> Bound.maximum
    |> Bound.(max zero)
    |> tap (fun starting_value -> Logger.log logger Logger.DEBUG
                                             (fun () -> "starting_value", ["result", Bound.to_string starting_value]))
  in
    
  Bound.(sign kind (loop_scaling_factor * (starting_value kind + loop_effect)))

            
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
