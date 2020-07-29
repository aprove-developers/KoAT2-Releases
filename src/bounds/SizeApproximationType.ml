open Batteries
open ProgramTypes
open RVGTypes

type kind = [ `Lower | `Upper ] [@@deriving eq, ord, show]

module Make_SizeApproximation (Num : PolyTypes.OurNumber) (Poly :
                                 sig
                                   include PolyTypes.Polynomial with type value = Num.t
                                                                 and type valuation = Valuation.Make(Num).t
                                                                 and type monomial = Monomials.Make(Num).t
                                   val max_of_occurring_constants : t -> Num.t
                                 end )
                              (Trans :
                                 sig
                                   type t
                                   val same: t -> t -> bool
                                   val src: t -> Location.t
                                   val target_string: t -> string
                                   val to_id_string: t -> string
                                   val compare_same: t -> t -> int
                                 end)
                              (RV :
                                 sig
                                   type t = Trans.t * Var.t
                                   val to_id_string: t -> string
                                 end) =
  struct
    module B = BoundType.Make_BoundOver (Num) (Poly)
    let logger = Logging.(get Approximation)

    module Map =
      Hashtbl.Make(
          struct
            type t = kind * Trans.t * Var.t

            let equal (kind1, t1, v1) (kind2, t2, v2) =
              equal_kind kind1 kind2
              && Trans.same t1 t2
              && Var.equal v1 v2

            let hash (kind, t, v) =
              Hashtbl.hash (show_kind kind
                            ^ Location.to_string (Trans.src t) ^ Trans.target_string t
                            ^ Var.to_string v)
          end
        )

    type t = B.t Map.t

    let empty = Map.create

    let keep_minimum_degree_bound b1 b2 =
      if B.compare_complexity (B.asymptotic_complexity b1) (B.asymptotic_complexity b2) = -1 then
        b1
      else
        b2

    let keep_simplest_bound b1 b2 =
      let compl_comp = B.compare_complexity (B.asymptotic_complexity b1) (B.asymptotic_complexity b2)  in
      let comp_comp_1 = B.(b1 < b2) in
      let comp_comp_2 = B.(b2 < b1) in
      let str_length_comp = (String.length @@ B.to_string b1) <= (String.length @@ B.to_string b2) in
      match (compl_comp, comp_comp_1, comp_comp_2, str_length_comp) with
      (* b1 < b2 *)
      | (_, Some true,_, _) -> b1
      (* b2 < b1 *)
      | (_, _, Some true, _) -> b2
      (* compl b1 < compl_b2 *)
      | (-1,_,_,_) -> b1
      (* compl b2 < compl_b1 *)
      | (1,_,_,_) -> b2
      (* length b1 < length b2 *)
      | (_,_,_,true) -> b1
      (* length b2 < length b1 *)
      | (_,_,_,false) -> b2

    (* Returns the operator to combine two bounds with the best result. *)
    let combine_bounds ?(simplifyfunc = identity) kind b1 b2 = match kind with
      | `Lower -> simplifyfunc (keep_simplest_bound b1 b2)
      | `Upper -> simplifyfunc (keep_simplest_bound b1 b2)

    let get kind map transition var =
      let execute () =
        Map.find_option map (kind, transition, var)
        |? match kind with
           | `Lower -> B.minus_infinity
           | `Upper -> B.infinity
      in Logger.with_log logger Logger.DEBUG
                         (fun () -> "sizebound", ["kind", show_kind kind;
                                                  "rv", RV.to_id_string (transition, var)])
                         ~result:B.to_string
                         execute

    let add ?(simplifyfunc=identity) kind bound transition var map =
      let is_trivial = function
        | `Lower -> B.is_minus_infinity
        | `Upper -> B.is_infinity
      in
      (* We do not want to log trivial size bounds *)
      if not (is_trivial kind bound) then
        ( try
            let old_bound =  Map.find map (kind,transition,var) in
            Map.modify (kind, transition, var) (combine_bounds ~simplifyfunc:simplifyfunc kind bound) map;
            let new_bound =  Map.find map (kind,transition,var) in
            Logger.log logger Logger.DEBUG
              (fun () -> "modified_size_bound", ["kind", show_kind kind; "rv", RV.to_id_string (transition, var); "bound", B.to_string bound; "old_bound", B.to_string old_bound; "new_bound", B.to_string new_bound])
          with
          | Not_found -> (
            Map.add map (kind, transition, var) (simplifyfunc bound);
            Logger.log logger Logger.INFO
              (fun () -> "add_size_bound", ["kind", show_kind kind; "rv", RV.to_id_string (transition, var); "bound", B.to_string bound])
        ));
      map

    let add_all ?(simplifyfunc=identity) kind bound scc map =
      List.iter (fun (t,v) -> ignore (add ~simplifyfunc:simplifyfunc kind bound t v map)) scc;
      map

    let add_all_abs ?(simplifyfunc=identity) bound scc map =
      List.iter (fun (t,v) -> ignore (add ~simplifyfunc:simplifyfunc `Lower (B.zero) t v map); ignore (add ~simplifyfunc:simplifyfunc `Upper bound t v map)) scc;
      map

    let print_all_of_kind ?(html=false) ~show_kind_in_header output kind size =
      let sep = if html then "<br>" else "\n" in
      size
      |> Map.filteri (fun (k, _, _) _ -> equal_kind k kind)
      |> Map.to_list
      |> List.sort (fun ((_,t1,v1),b1) ((_,t2,v2),b2) ->
             if Trans.compare_same t1 t2 != 0 then
               Trans.compare_same t1 t2
             else
               Var.compare v1 v2
           )
      |> List.print
           ~first:(if show_kind_in_header then "  " else (show_kind kind ^ ":" ^ sep ^ "  "))
           ~last:sep
           ~sep:(sep ^ "  ")
           (fun output ((_, transition, var), bound) -> IO.nwrite output (Trans.to_id_string transition ^ ", " ^ Var.to_string var ^ ": " ^ B.to_string bound))
           output

    let to_string ?(html=false) ?(print_lower=true) size =
      let output = IO.output_string () in
      if print_lower then print_all_of_kind ~show_kind_in_header:(not print_lower) output `Lower size;
      print_all_of_kind ~html:html output ~show_kind_in_header:(not print_lower) `Upper size;
      IO.close_out output

    (** Very slow equality, only for testing purposes *)
    let equivalent size1 size2 =
      let module Set =
        Set.Make(struct type t = (kind * Trans.t * Var.t) * B.t
                        let compare ((kind1,t1,v1),bound1) ((kind2,t2,v2),bound2) =
                          if compare_kind kind1 kind2 != 0 then
                            compare_kind kind1 kind2
                          else if Trans.compare_same t1 t2 != 0 then
                            Trans.compare_same t1 t2
                          else if Var.compare v1 v2 != 0 then
                            Var.compare v1 v2
                          else if B.(bound1 < bound2) |? false then
                            -1
                          else if B.(bound1 > bound2) |? false then
                            1
                          else
                            0
                 end)
      in
      let to_set time = time |> Map.enum |> Set.of_enum in
      Set.equal (to_set size1) (to_set size2)

  end
