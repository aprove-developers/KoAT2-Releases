open Batteries
open ProgramTypes
open RVGTypes
   
type kind = [ `Lower | `Upper ] [@@deriving eq, ord, show]

let logger = Logging.(get Approximation) 

module Map =
  Hashtbl.Make(
      struct
        type t = kind * Transition.t * Var.t
               
        let equal (kind1, t1, v1) (kind2, t2, v2) =
          equal_kind kind1 kind2
          && Transition.same t1 t2
          && Var.equal v1 v2
          
        let hash (kind, t, v) =
          Hashtbl.hash (show_kind kind
                        ^ Location.to_string (Transition.src t) ^ Location.to_string (Transition.target t)
                        ^ Var.to_string v)
      end
    )
  
type t = Bound.t Map.t

let empty = Map.create
          
(* Returns the operator to combine two bounds with the best result. *)
let combine_bounds = function
  | `Lower -> Bound.max
  | `Upper -> Bound.min
            
let get kind map transition var =
  let execute () =
    Map.find_option map (kind, transition, var)
    |? match kind with
       | `Lower -> Bound.minus_infinity
       | `Upper -> Bound.infinity       
  in Logger.with_log logger Logger.DEBUG
                     (fun () -> "sizebound", ["kind", show_kind kind;
                                              "rv", RV.to_id_string (transition, var)])
                     ~result:Bound.to_string
                     execute

let add kind bound transition var map =
  let is_trivial = function
    | `Lower -> Bound.is_minus_infinity
    | `Upper -> Bound.is_infinity
  in
  (* We do not want to log trivial size bounds *)
  if not (is_trivial kind bound) then
    ( try
        Map.modify (kind, transition, var) (combine_bounds kind bound) map;
        Logger.log logger Logger.DEBUG
          (fun () -> "modified_size_bound", ["kind", show_kind kind; "rv", RV.to_id_string (transition, var); "bound", Bound.to_string bound])
      with
      | Not_found -> (
        Map.add map (kind, transition, var) bound;
        Logger.log logger Logger.INFO
          (fun () -> "add_size_bound", ["kind", show_kind kind; "rv", RV.to_id_string (transition, var); "bound", Bound.to_string bound])
    ));
  map

let add_all kind bound scc map =
  List.iter (fun (t,v) -> ignore (add kind bound t v map)) scc;
  map

let print_all_of_kind html output kind size =
  let sep = if html then "<br>" else "\n" in
    size
    |> Map.filteri (fun (k, _, _) _ -> equal_kind k kind)
    |> Map.to_list
    |> List.sort (fun ((_,t1,v1),b1) ((_,t2,v2),b2) ->
          if Transition.compare_same t1 t2 != 0 then
            Transition.compare_same t1 t2
          else
            Var.compare v1 v2
        )
    |> List.print
        ~first:(show_kind kind ^ ":" ^ sep)
        ~last:sep
        ~sep:(sep^"  ")
        (fun output ((_, transition, var), bound) -> IO.nwrite output (Transition.to_id_string transition ^ ", " ^ Var.to_string var ^ ": " ^ Bound.to_string bound))
        output
  
let to_string ?(html=false) size =
  let output = IO.output_string () in
  print_all_of_kind html output `Lower size;
  print_all_of_kind html output `Upper size;
  IO.close_out output

(** Very slow equality, only for testing purposes *)
let equivalent size1 size2 =
  let module Set =
    Set.Make(struct type t = (kind * Transition.t * Var.t) * Bound.t
                    let compare ((kind1,t1,v1),bound1) ((kind2,t2,v2),bound2) =
                      if compare_kind kind1 kind2 != 0 then
                        compare_kind kind1 kind2
                      else if Transition.compare_same t1 t2 != 0 then
                        Transition.compare_same t1 t2
                      else if Var.compare v1 v2 != 0 then
                        Var.compare v1 v2
                      else if Bound.(bound1 < bound2) |? false then
                        -1
                      else if Bound.(bound1 > bound2) |? false then
                        1
                      else
                        0
             end)
  in
  let to_set time = time |> Map.enum |> Set.of_enum in
  Set.equal (to_set size1) (to_set size2)
  
