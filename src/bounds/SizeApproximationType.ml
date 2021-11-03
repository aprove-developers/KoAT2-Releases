open Batteries
open ProgramTypes
open RVGTypes

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
                                   val id: t -> int
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
            type t = Trans.t * Var.t

            let equal (t1, v1) (t2, v2) =
              Trans.id t1 = Trans.id t2
              && Var.equal v1 v2

            let hash (t, v) =
              Hashtbl.hash (Int.to_string (Trans.id t)
                            ^ Var.to_string v)
          end
        )

    type t = B.t Map.t

    let empty = Map.create

    let get map transition var =
      let execute () =
        Map.find_option map (transition, var)
        |? B.infinity
      in Logger.with_log logger Logger.DEBUG
                         (fun () -> "sizebound", ["rv", RV.to_id_string (transition, var)])
                         ~result:B.to_string
                         execute

    let add ?(simplifyfunc=identity) bound transition var map =
      (* We do not want to log trivial size bounds *)
      if not (B.is_infinity bound) then
        ( try
            let old_bound =  Map.find map (transition,var) in
            Map.modify (transition, var) (simplifyfunc % B.keep_simpler_bound bound) map;
            let new_bound =  Map.find map (transition,var) in
            Logger.log logger Logger.DEBUG
              (fun () -> "modified_size_bound", ["rv", RV.to_id_string (transition, var); "bound", B.to_string bound; "old_bound", B.to_string old_bound; "new_bound", B.to_string new_bound])
          with
          | Not_found -> (
            Map.add map (transition, var) (simplifyfunc bound);
            Logger.log logger Logger.INFO
              (fun () -> "add_size_bound", ["rv", RV.to_id_string (transition, var); "bound", B.to_string bound])
        ));
      map

    let add_all ?(simplifyfunc=identity) bound scc map =
      List.iter (fun (t,v) -> ignore (add ~simplifyfunc:simplifyfunc bound t v map)) scc;
      map

    let to_formatted ?(pretty=false) size =
      Map.to_list size
      |> List.sort (fun ((t1,v1),b1) ((t2,v2),b2) ->
             if Trans.compare_same t1 t2 != 0 then
               Trans.compare_same t1 t2
             else
               Var.compare v1 v2
           )
      |> List.map
           (fun ((transition, var), bound) -> FormattedString.mk_str_line @@ "  t" ^ (if pretty then Trans.id transition |> Util.natural_to_subscript else Trans.id transition |> string_of_int) ^ ", " ^ Var.to_string ~pretty var ^ ": " ^ B.to_string ~pretty bound)
      |> FormattedString.mappend

    let to_string size =
      FormattedString.render_string @@ to_formatted size

    (** Very slow equality, only for testing purposes *)
    let equivalent size1 size2 =
      let module Set =
        Set.Make(struct type t = (Trans.t * Var.t) * B.t
                        let compare ((t1,v1),bound1) ((t2,v2),bound2) =
                          if Trans.compare_same t1 t2 != 0 then
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
