open! OurBase

module ModifierComparator_ = struct
  open GenericModifier_

  type 'a t = 'a modifier_t_

  let compare trans_compare x y =
    match (x, y) with
    | TR _, VR _ -> -1
    | VR _, TR _ -> 1
    | VR v1, VR v2 -> VarFunctionCall.compare v1 v2
    | TR t1, TR t2 -> trans_compare t1 t2


  let sexp_of_t _ = Sexplib0.Sexp_conv.sexp_of_opaque
end

module ModifierComparator = Comparator.Derived (ModifierComparator_)

module type Adapter2PolyRec = sig
  type t

  val convert : t -> PolyRec.PolyRec.t
end

module Modifier
    (TL : ProgramTypes.TransitionLabel)
    (A : Adapter2PolyRec with type t = TL.update_element)
    (T : ProgramTypes.Transition with type transition_label = TL.t) =
struct
  module Inner = struct
    open GenericModifier_

    type t = T.t ModifierComparator_.t

    let comparator = ModifierComparator.comparator T.comparator
    let equal = Comparator.equal_of_comparator comparator

    type comparator_witness = T.comparator_witness ModifierComparator.comparator_witness

    let of_transition t = TR t
    let of_function_call fc = VR fc

    let to_transition = function
      | TR t -> t
      | VR v -> failwith "VarFunctionCall cannot be transformed into transition!"


    let to_function_call = function
      | TR t -> failwith "Transition cannot be transformed into VarFunctionCall!"
      | VR v -> v


    let is_transition = function
      | TR t -> true
      | VR v -> false


    let to_id_string = function
      | TR t -> T.to_id_string t
      | VR v -> VarFunctionCall.to_string v


    let ids_to_string ?(pretty = false) = function
      | TR t -> TL.ids_to_string ~pretty (T.label t)
      | VR v -> VarFunctionCall.to_string v


    open PolyRec

    let update m v =
      match m with
      | TR t ->
          let opt = TL.update (T.label t) v in
          if Option.is_some opt then
            A.convert @@ Option.value_exn opt
          else
            PolyRec.of_var v
      | VR fc ->
          PolyRec.of_poly
          @@ Map.find_default (VarFunctionCall.update fc) ~default:(Polynomials.Polynomial.of_var v) v


    let hash = function
      | TR t -> T.hash t
      | VR fc -> VarFunctionCall.hash fc
  end

  include Inner
end

module MakeRV
    (TL : ProgramTypes.TransitionLabel)
    (A : Adapter2PolyRec with type t = TL.update_element)
    (T : ProgramTypes.Transition with type transition_label = TL.t) =
struct
  type transition = T.t

  module M = Modifier (TL) (A) (T)

  type modifier = M.t
  type t = M.t * Var.t
  type comparator_witness_modifier = M.comparator_witness

  let hash (m, v) = Hashtbl.hash (M.hash m, Var.to_string v)
  let modifier_of_transition = M.of_transition
  let modifier_of_function_call = M.of_function_call
  let transition (t, _) = M.to_transition t
  let function_call (fc, _) = M.to_function_call fc
  let function_call_ fc = M.to_function_call fc
  let transition_ = M.to_transition
  let modifier (m, _) = m
  let variable (_, v) = v
  let to_id_string (m, v) = "|" ^ M.to_id_string m ^ "," ^ Var.to_string v ^ "|"
  let ids_to_string ?(pretty = false) (m, v) = M.ids_to_string ~pretty m ^ ", " ^ Var.to_string ~pretty v
  let sexp_of_t = Sexplib0.Sexp_conv.sexp_of_opaque

  type comparator_witness =
    (comparator_witness_modifier, Var.comparator_witness) RVComparator.comparator_witness

  let comparator = RVComparator.comparator M.comparator Var.comparator
  let compare = Comparator.compare_of_comparator comparator
  let equal = Comparator.equal_of_comparator comparator
  let equal_modifier = M.equal
  let to_generic_modifier = identity
  let update (m, _) v = M.update m v
  let has_transition (m, _) = M.is_transition m
  let is_transition m = M.is_transition m
end

module IdentityAdapter = struct
  type t = PolyRec.PolyRec.t

  let convert = identity
end

module RV = MakeRV (TransitionLabel_) (IdentityAdapter) (Transition_)

module Edge = struct
  type t = NORMAL | RETURN

  let default = NORMAL

  let compare t1 t2 =
    match (t1, t2) with
    | NORMAL, RETURN -> -1
    | RETURN, NORMAL -> 1
    | _ -> 0
end

module MakeRVG (PM : ProgramTypes.ClassicalProgramModules) = struct
  open PM
  module RV = PM.RV
  module G = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled (PM.RV) (Edge)
  module C = Graph.Components.Make (G)
  include G

  type rv = RV.t
  type scc = RV.t list

  let rvs_to_id_string rvs = rvs |> List.map ~f:RV.to_id_string |> String.concat ~sep:","

  let pre rvg rv =
    pred_e rvg rv
    |> List.filter_map ~f:(fun (v, e, _) ->
           match e with
           | Edge.NORMAL -> Option.return v
           | _ -> None)


  let pre_omega rvg rv =
    pred_e rvg rv
    |> List.filter_map ~f:(fun (v, e, _) ->
           match e with
           | Edge.RETURN -> Option.return v
           | _ -> None)


  let add_vertices_to_rvg vertices rvg = Sequence.fold ~f:add_vertex ~init:rvg vertices

  let rvg_from_transitionset (get_vars_in_lsb : rv -> VarFunctionCallSet.t Option.t) program tset =
    let program_vars = Program.input_vars program in
    let add_transition rvg post_transition =
      let function_calls_of_post_transition = Transition.rec_vars post_transition in
      let rvg_with_vertices : t =
        add_vertices_to_rvg
          (Set.to_sequence program_vars
          |> Sequence.map ~f:(fun var ->
                 Sequence.shift_right
                   (Set.to_sequence
                   @@ Set.map
                        (module RV)
                        function_calls_of_post_transition
                        ~f:(fun fc -> (RV.modifier_of_function_call fc, var)))
                   (RV.modifier_of_transition post_transition, var))
          |> Sequence.join)
          rvg
      in
      let pre_transitions = Set.inter (Program.pre_without_rec program post_transition) tset in
      let pre_nodes transition (post_var : Var.t) =
        let vars_in_lsb (modifier, post_var) =
          get_vars_in_lsb (modifier, post_var)
          |? VarFunctionCallSet.empty
          |> Set.filter ~f:(not % VarFunctionCall.is_function_call)
          |> VarSet.map ~f:VarFunctionCall.to_var
        in
        let fc_in_lsb =
          get_vars_in_lsb (RV.modifier_of_transition transition, post_var)
          |? VarFunctionCallSet.empty
          |> Set.filter ~f:VarFunctionCall.is_function_call
        in

        (* All RV pairs which result of a transition -> transition *)
        let transition_transition =
          vars_in_lsb (RV.modifier_of_transition transition, post_var)
          |> Set.to_sequence
          |> Sequence.cartesian_product (Set.to_sequence pre_transitions)
          |> Sequence.map ~f:(fun (pre_transition, pre_var) ->
                 ( RV.modifier_of_transition pre_transition,
                   pre_var,
                   Edge.NORMAL,
                   RV.modifier_of_transition transition,
                   post_var ))
        in
        (* All RV pairs which result of a transition t' -> fc. Here, fc occurs on a transition t s.t. t' is a pre transition of t. *)
        let transition_fc =
          Set.to_sequence @@ Program.input_vars program
          |> Sequence.cartesian_product (fc_in_lsb |> Set.to_sequence)
          |> Sequence.cartesian_product (Set.to_sequence pre_transitions)
          |> Sequence.map ~f:(fun (pre_transition, (fc, pre_var)) ->
                 let active_vars = Set.to_list @@ vars_in_lsb (RV.modifier_of_function_call fc, pre_var) in
                 Sequence.of_list
                 @@ List.map active_vars ~f:(fun v ->
                        ( RV.modifier_of_transition pre_transition,
                          v,
                          Edge.NORMAL,
                          RV.modifier_of_function_call fc,
                          pre_var )))
          |> Sequence.join
        in
        let function_calls =
          List.map (Set.to_list tset) ~f:Transition.rec_vars |> VarFunctionCallSet.union_list
        in
        let fc_transition =
          vars_in_lsb (RV.modifier_of_transition transition, post_var)
          |> Set.to_sequence
          |> Sequence.map ~f:(fun pre_var ->
                 Set.to_sequence function_calls
                 |> Sequence.filter_map ~f:(fun fc ->
                        if Location.equal (VarFunctionCall.return_loc fc) (Transition.src transition) then
                          Option.return
                            ( RV.modifier_of_function_call fc,
                              pre_var,
                              Edge.NORMAL,
                              RV.modifier_of_transition transition,
                              post_var )
                        else
                          None))
          |> Sequence.join
        in
        (* All RV pairs which result of a fc -> fc'. Here, fc jumps to the start location of a transition of fc'. *)
        let fc_fc =
          Set.to_sequence @@ Program.input_vars program
          |> Sequence.cartesian_product (fc_in_lsb |> Set.to_sequence)
          |> Sequence.cartesian_product (Set.to_sequence function_calls)
          |> Sequence.map ~f:(fun (pre_fc, (fc, pre_var)) ->
                 if Location.equal (VarFunctionCall.return_loc pre_fc) (Transition.src post_transition) then
                   let active_vars = Set.to_list @@ vars_in_lsb (RV.modifier_of_function_call fc, pre_var) in
                   Sequence.of_list
                   @@ List.map active_vars ~f:(fun v ->
                          ( RV.modifier_of_function_call pre_fc,
                            v,
                            Edge.NORMAL,
                            RV.modifier_of_function_call fc,
                            pre_var ))
                 else
                   Sequence.empty)
          |> Sequence.join
        in
        let return_edges =
          if Set.mem (Program.return_locations program) (Transition.target post_transition) then
            Set.to_sequence @@ Program.input_vars program
            |> Sequence.cartesian_product (Set.to_sequence @@ tset)
            |> Sequence.map ~f:(fun (t, v) ->
                   let function_calls =
                     TransitionLabel.update (Transition.label t) v
                     |? PolyRec.PolyRec.of_var v |> PolyRec.PolyRec.rec_vars
                   in
                   List.filter_map function_calls ~f:(fun fc ->
                       if
                         Set.mem
                           (Program.reachable_locations program (VarFunctionCall.return_loc fc))
                           (Transition.target post_transition)
                       then
                         Option.return
                           ( RV.modifier_of_transition post_transition,
                             VarFunctionCall.return_var fc,
                             Edge.RETURN,
                             RV.modifier_of_transition t,
                             v )
                       else
                         None)
                   |> Sequence.of_list)
            |> Sequence.join
          else
            Sequence.empty
        in
        transition_transition |> Sequence.append transition_fc |> Sequence.append fc_transition
        |> Sequence.append fc_fc |> Sequence.append return_edges
      in

      program_vars |> Set.to_sequence
      |> Sequence.map ~f:(fun post_var -> pre_nodes post_transition post_var)
      |> Sequence.join
      |> Sequence.fold
           ~f:(fun rvg (pre_modifier, pre_var, label, modifier, post_var) ->
             add_edge_e rvg ((pre_modifier, pre_var), label, (modifier, post_var)))
           ~init:rvg_with_vertices
    in
    Set.fold ~init:empty ~f:add_transition tset


  let rvg get_vars_in_lsb program =
    rvg_from_transitionset get_vars_in_lsb program (Program.transitions program)


  let rvg_with_sccs get_vars_in_lsb program =
    let rvg = rvg get_vars_in_lsb program in
    (rvg, Lazy.from_fun (fun () -> C.scc_list rvg))


  let rvg_from_transitionset_with_sccs get_vars_in_lsb program scc =
    let rvg = rvg_from_transitionset get_vars_in_lsb program scc in
    (rvg, Lazy.from_fun (fun () -> C.scc_list rvg))
end
