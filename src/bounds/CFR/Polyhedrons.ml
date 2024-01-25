open! OurBase

module Polyhedrons = struct
  open Constraints
  open Polynomials

  (*
    For an abstract domain `'a` there is a manager `'a Manager.t` handling the library stuff underneath.
    It is not important to this implementation, but is required for most of the operations. We will use the Ppl abstract domain for partial evaluation, but others might be used as well.

    A value in the abstract domain `'a` is an `'a Abstract.t`.
    It holds a reference to the environment (variables).
  *)

  (** Add new dimensions to to an abstract value, ignores already existing dimensions. *)
  let add_vars_to_polyh am vars polyh =
    let prev_env = Apron.Abstract1.env polyh in
    let prev_vars = Apron.Environment.vars prev_env |> fst |> ApronInterface.Apron2Koat.vars_from_apron in
    let new_vars = Set.filter ~f:(fun var -> not (Set.mem prev_vars var)) vars in
    let new_env = Apron.Environment.add prev_env (ApronInterface.Koat2Apron.vars_to_apron new_vars) [||] in
    Apron.Abstract1.change_environment am polyh new_env false


  (**
    Projects a polyhedron onto the given variables.
    For variables, which are not already in the polyhedron, no new dimensions have to be added beforewards.
   *)
  let project_polyh am vars polyh =
    let prev_env = Apron.Abstract1.env polyh in
    let apron_vars =
      ApronInterface.Koat2Apron.vars_to_apron vars |> Array.filter ~f:(Apron.Environment.mem_var prev_env)
    in
    let new_env = Apron.Environment.make apron_vars [||] in
    (* Implicitly projects all removed dimensions *)
    Apron.Abstract1.change_environment am polyh new_env false


  let project_constraint am vars guard =
    ApronInterface.Koat2Apron.constraint_to_polyh am guard
    |> project_polyh am vars
    |> ApronInterface.Apron2Koat.polyh_to_constraint am


  (** Intersect a polyhedron with a constraint. *)
  let intersect_constraint am constr polyh =
    let env = Apron.Abstract1.env polyh in
    let apron_expr = ApronInterface.Koat2Apron.constraint_to_apron env constr in
    Apron.Abstract1.meet_tcons_array am polyh apron_expr


  (** Find bounds of the form x ⋄ c for variables x and bounds c in a given polyhedron. *)
  let bound_variables_polyh am vars polyh =
    let env = Apron.Abstract1.env polyh in
    Set.to_sequence vars
    |> Sequence.map ~f:(fun variable ->
           let apron_var = ApronInterface.Koat2Apron.var_to_apron variable in
           if Apron.Environment.mem_var env apron_var then
             Apron.Abstract1.bound_variable am polyh apron_var
             |> ApronInterface.Apron2Koat.interval_from_apron variable
           else
             Guard.mk_true)
    |> Sequence.fold ~f:Constraint.mk_and ~init:Constraint.mk_true


  (** Find bounds of the form x ⋄ c for variables x and bounds c in a given constraint. *)
  let bound_variables_constraint am vars constr =
    ApronInterface.Koat2Apron.constraint_to_polyh am constr |> bound_variables_polyh am vars


  let vars_in_update update =
    Map.fold
      ~f:(fun ~key:var ~data:ue vars -> Set.add vars var |> Set.union (Polynomials.Polynomial.vars ue))
      update ~init:VarSet.empty


  let update_polyh am update polyh =
    let vars_in_update =
      Map.fold ~f:(fun ~key:_ ~data:poly -> Set.union (Polynomial.vars poly)) update ~init:VarSet.empty
      |> Set.union (VarSet.of_list @@ Map.keys update)
    in
    let polyh_with_new_vars = add_vars_to_polyh am vars_in_update polyh in
    let env = Apron.Abstract1.env polyh_with_new_vars in
    let vars_arr, texpr_arr = ApronInterface.Koat2Apron.update_to_apron env update in
    Apron.Abstract1.assign_texpr_array am polyh_with_new_vars vars_arr texpr_arr None


  let update_guard am update guard =
    ApronInterface.Koat2Apron.constraint_to_polyh am guard
    |> update_polyh am update
    |> ApronInterface.Apron2Koat.polyh_to_constraint am
end
