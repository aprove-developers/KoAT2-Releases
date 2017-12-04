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

  (** All transitions that are present in the scc. *)
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

  (** Returns all result variables that may influence the given result variable from within the scc. *)
  let scc_variables rv =
    rv
    |> pre_in_scc
    |> Enum.map (fun (t,v) -> v)
    |> Enum.uniq_by Var.equal
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
    let execute () =
      transitions
      |> List.enum
      |> Enum.map transition_scaling_factor
      |> Bound.product
    in Logger.with_log logger Logger.DEBUG
                       (fun () -> "overall scaling factor", [])
                       ~result:Bound.to_string
                       execute
  in

  let rv_prevar_effect prekind rv v =
    let execute () =
      rv
      |> pre_out_scc
      |> Enum.filter (fun (_,v') -> Var.equal v v')
      |> Enum.map (fun (t,v) -> get_sizebound prekind t v)
      |> Enum.map (sign prekind)
      |> Bound.maximum
      |> Bound.(max zero)
    in Logger.with_log logger Logger.DEBUG
                       (fun () -> "rv prevar effect", ["prekind", show_kind kind;
                                                       "rv", RV.to_id_string rv;
                                                       "variable", Var.to_string v])
                       ~result:Bound.to_string
                       execute
  in
  
  let rv_effect sign rv =
    let execute () =
      rv
      |> get_lsb
      |> LocalSizeBound.vars_of_sign sign
      |> VarSet.enum
      |> Util.without Var.equal (scc_variables rv)
      |> Enum.map (rv_prevar_effect (LocalSizeBound.pre_kind (kind, sign)) rv)
      |> Bound.sum
    in Logger.with_log logger Logger.DEBUG
                       (fun () -> "rv effect", ["sign", show_sign sign;
                                                "rv", RV.to_id_string rv])
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
      |> Bound.(max zero)
    in Logger.with_log logger Logger.DEBUG
                       (fun () -> "rv constant", ["variable", RV.to_id_string (t,v)])
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
             Bound.(rv_constant (t,v) + (rv_effect `Pos (t,v) + rv_effect `Neg (t,v)))
           )
      |> Bound.maximum
    in Logger.with_log logger Logger.DEBUG
                       (fun () -> "transition effect", ["transition", Transition.to_id_string t])
                       ~result:Bound.to_string
                       execute
  in

  (** The effects inside the scc of all transitions all together. *)
  let effects =
    let execute () =
      transitions
      |> List.enum
      |> Enum.map (fun t -> Bound.(get_timebound t * transition_effect t))
      |> Bound.sum
    in Logger.with_log logger Logger.DEBUG
                       (fun () -> "effects", [])
                       ~result:Bound.to_string
                       execute
  in

  (** The maximum of the highest positive value that can enter the scc and the amount of the lowest negative value that can enter the scc.
      Corresponds to the definition of 'e'. *)
  let start_value =
    scc
    |> List.enum
    |> Enum.map pre_out_scc
    |> Enum.flatten
    |> Enum.map (fun (t',v') ->
           [
             get_sizebound `Upper t' v';
             get_sizebound `Lower t' v' |> Bound.neg;
           ]
           |> List.enum
         )
    |> Enum.flatten
    |> Bound.maximum
    |> Bound.(max zero)
    |> tap (fun start_value -> Logger.log logger Logger.DEBUG (fun () -> "start_value", ["result", Bound.to_string start_value]))
  in
    
  Bound.(sign kind (overall_scaling_factor * (start_value + effects)))

            
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
