open Batteries
open Program.Types
   
type kind = [ `Lower | `Upper ] [@@deriving eq, ord, show]

let logger = Logger.make_log "appr"

module Time =
  struct
    module Map = Hashtbl.Make(Transition)
               
    type t = Bound.t Map.t
           
    let empty = Map.create
              
    let get map transition =
      let execute () =
        Map.find_option map transition |? Bound.infinity
      in Logger.with_log logger Logger.DEBUG
                         (fun () -> "timebound", ["transition", Transition.to_id_string transition])
                         ~result:Bound.to_string
                         execute

    let get_between map src target =
      map
      |> Map.filteri (fun (l,_,l') _ -> Location.(equal l src && equal l' target))
      |> Map.values
      |> Option.some
      |> Option.filter (fun values -> Enum.count values = 1)
      |> fun option -> Option.bind option Enum.get

    let sum map program =
      TransitionGraph.fold_edges_e (fun transition result -> Bound.(of_poly (Transition.cost transition) * get map transition + result)) (Program.graph program) Bound.zero

    let sum_available map =
      Map.fold (fun transition bound result -> Bound.(bound + result)) map Bound.zero
      
    let add bound transition map =
      (try
         Map.modify transition (Bound.min bound) map
       with Not_found -> Map.add map transition bound);
      map

    let all_bounded map =
      List.for_all (fun t -> not (Bound.equal (get map t) Bound.infinity))
      
    let to_string time =
      let output = IO.output_string () in
      Map.print
        ~first:("  ")
        ~last:"\n"
        ~sep:"\n  "
        (fun output transition -> IO.nwrite output (Transition.to_id_string transition))
        (fun output bound -> IO.nwrite output (Bound.to_string bound))
        output time;  
      IO.close_out output

    (** Very slow equality, only for testing purposes *)
    let equal time1 time2 =
      let module Set =
        Set.Make(struct type t = Transition.t * Bound.t [@@deriving ord] end)
      in
      let to_set time = time |> Map.enum |> Set.of_enum in
      Set.equal (to_set time1) (to_set time2)

  end

module Size =
  struct
    module Map =
      Hashtbl.Make(
          struct
            type t = kind * Transition.t * Var.t [@@deriving eq]
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
      (try
         Map.modify (kind, transition, var) (combine_bounds kind bound) map
       with Not_found -> Map.add map (kind, transition, var) bound);
      map

    let add_all kind bound scc map =
      List.iter (fun (t,v) -> ignore (add kind bound t v map)) scc;
      map

    let print_all_of_kind output kind size =
      size
      |> Map.filteri (fun (k, _, _) _ -> equal_kind k kind)
      |> Map.print
           ~first:(show_kind kind ^ ":\n  ")
           ~last:"\n"
           ~sep:"\n  "
           (fun output (_, transition, var) -> IO.nwrite output (Transition.to_id_string transition ^ ", " ^ Var.to_string var))
           (fun output bound -> IO.nwrite output (Bound.to_string bound))
           output
      
    let to_string size =
      let output = IO.output_string () in
      print_all_of_kind output `Lower size;
      print_all_of_kind output `Upper size;
      IO.close_out output

    (** Very slow equality, only for testing purposes *)
    let equal size1 size2 =
      let module Set =
        Set.Make(struct type t = (kind * Transition.t * Var.t) * Bound.t [@@deriving ord] end)
      in
      let to_set time = time |> Map.enum |> Set.of_enum in
      Set.equal (to_set size1) (to_set size2)
            
  end

type t = Time.t * Size.t [@@deriving eq]

let empty transitioncount varcount =
    Time.empty transitioncount,
    Size.empty (2 * transitioncount * varcount)

let time (time, _) = time

let size (_, size) = size

let timebound (time, _) = Time.get time

let timebound_between (time, _) = Time.get_between time
                        
let add_timebound bound transition = Tuple2.map1 (Time.add bound transition)

let all_times_bounded (time, _) = Time.all_bounded time
                                   
let sizebound kind (_, size) = Size.get kind size

let add_sizebound kind bound transition var = Tuple2.map2 (Size.add kind bound transition var)

let add_sizebounds kind bound scc = Tuple2.map2 (Size.add_all kind bound scc)

let to_string program (time, size) =
  let output = IO.output_string () in
  IO.nwrite output "Timebounds: \n";
  IO.nwrite output ("  Overall timebound: " ^ Bound.to_string (Time.sum time program) ^ "\n");
  time |> Time.to_string |> IO.nwrite output;
  IO.nwrite output "\nSizebounds:\n";
  size |> Size.to_string |> IO.nwrite output;
  IO.close_out output
