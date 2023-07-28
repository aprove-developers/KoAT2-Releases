(** Provides implemenation of transition and program generation. *)
open OurBase
open Formulas
open Polynomials
open ProgramModules

exception Error of string
exception ComKindAndTargetsMismatch
exception OnlyCom1InSimpleMode

(** Hashtable for caching the arities of locations while reading **)
let location_table: (Location.t,int) Hashtbl.t = Hashtbl.create ~size:20 (module Location)

let get_com_kind_from_com_str str =
  Int.of_string (String.drop_prefix str 4)

let empty_cache () =
  Hashtbl.clear location_table

let check_arity (loc: Location.t) (arity: int): unit =
  let stored_arity = Hashtbl.find location_table loc
  and string_of_stored_arities = location_table
                    |> Hashtbl.to_sequence
                    |> Util.sequence_to_string ~f:(fun (location, arity) -> Location.to_string location ^ ": " ^ string_of_int arity) in
  match stored_arity with
    | None -> Hashtbl.add_exn location_table ~key:loc ~data:arity
    | (Some m) -> if (m == arity) then () else raise (Error ("Location " ^ (Location.to_string loc) ^" occurs with different arities of variables "^ (string_of_int m) ^ "<>" ^ (string_of_int arity) ^ " !" ^ "\n Stored arities: " ^ string_of_stored_arities))

(** Generates transitions from given parameters *)
let mk_transition lhs (cost: Polynomial.t) rhs (formula: Formula.t) (vars:Var.t list) =
  let targets = Tuple2.second rhs in
  let start_loc = Tuple2.first lhs in
  let var_list = Tuple2.second lhs in
  let com_kind = get_com_kind_from_com_str @@ Tuple2.first rhs in
  if not (Int.equal com_kind (List.length targets)) then raise ComKindAndTargetsMismatch else
    check_arity (Location.of_string start_loc) (List.length var_list);
    List.iter
      ~f:(fun (target_loc, update_expr) -> check_arity (Location.of_string target_loc) (List.length update_expr))
      (Tuple2.second rhs);
    formula
    |> Formula.constraints
    |> List.map ~f:(fun constr ->
        List.map
          ~f:(fun (target_loc, update_expr) ->
            (Location.of_string start_loc,
              TransitionLabel.mk ~id:None ~assignments:update_expr ~guard:constr ~patterns:(List.map ~f:Var.of_string var_list) ~cost,
              Location.of_string target_loc)
          )
          (Tuple2.second rhs)
       )
    |> List.map ~f:(List.map ~f:(fun (l,t,l') -> (l,t,l')))

(** Returns list of default variables: x,y,z,u,v,w,p and q *)
let default_vars =
  ["x"; "y"; "z"; "u"; "v"; "w"; "p"; "q"]
  |> List.map ~f:Var.of_string

(** Input is not interpreted as a filepath, but as a program in simple mode. Method returns all transitions from such an input. *)
(** Assume Com_1 transitions *)
let mk_transition_simple (start: string) (cost: Polynomial.t) (rhs: string * (string * Polynomial.t list) list) (formula: Formula.t): Transition.t list =
  let com_kind = get_com_kind_from_com_str (Tuple2.first rhs) in
  if not (Int.equal com_kind 1) then raise OnlyCom1InSimpleMode else
    let (target_loc, updates) =  List.hd_exn @@ Tuple2.second rhs in
    formula
    |> Formula.constraints
    |> List.map ~f:(fun constr ->
           (Location.of_string start,
            TransitionLabel.mk
              ~id:None
              ~assignments:updates
              ~patterns:default_vars
              ~guard:constr
              ~cost:cost, Location.of_string target_loc)
         )

(** Input is not interpreted as a filepath, but as a program in simple mode. Method returns program from such an input. *)
let mk_program_simple (transitions: Transition.t list): Program.t =
  Program.from_sequence (Transition.src @@ List.hd_exn transitions) (Sequence.of_list transitions)

let ourfloat_of_decimal_or_fraction_string (str: string): OurFloat.t =
  (* Check if fraction *)
  if String.contains str '[' then
    OurFloat.of_string (String.drop_prefix str 1 |> flip String.drop_suffix 1)
  else
    let (str_before_point,str_after_point) = Option.value ~default:(str,"") (String.lsplit2 ~on:('.') str) in
    let numerator =
      if String.is_empty str_after_point then OurFloat.zero else OurFloat.of_string str_after_point
    in
    let denominator =
      OurFloat.pow (OurFloat.of_int 10) (String.length str_after_point)
    in
    let fractional = if String.is_empty str_after_point then OurFloat.zero else OurFloat.( numerator/denominator ) in
    let leading = if String.is_empty str_before_point then OurFloat.zero else OurFloat.of_string str_before_point in
    OurFloat.(leading + fractional)


(* Probabilistic Programs *)
open ProbabilisticProgramModules

let embed_probabilistic_transition_label lhs (probability, (com_kind, targets)) =
  let (start_loc, patterns) = Tuple2.map2 (List.map ~f:Var.of_string) lhs in

  check_arity (Location.of_string start_loc) (List.length patterns);
  List.iter ~f:(fun (loc, assignments) -> check_arity (Location.of_string loc) (List.length assignments)) targets;

  let com_nr = get_com_kind_from_com_str com_kind in
  com_nr,
  List.map ~f:(fun (loc,assignments) -> probability, assignments, loc) targets

let mk_general_transitions (gts: ((string * string list) * Polynomial.t * (int * (OurFloat.t * UpdateElement.t list * string) list) list * Formula.t) list) =
  let lhs_locations = Set.of_list (module String) @@ List.map ~f:(fun ((loc,_),_,_,_) -> loc) gts in
  let cleaned_com_k_transitions =
    List.map
      ~f:(Tuple4.map3 @@ List.map ~f:(fun (com_kind,t) ->
        (if com_kind <> List.length t then raise ComKindAndTargetsMismatch);
        let cleaned = List.filter ~f:(fun (_,_,target) -> Set.mem lhs_locations target) t in
        if List.length cleaned > 1 then raise Program_.RecursionNotSupported else List.hd_exn (cleaned@t)))
      gts
  in
  let number_patterns =
    List.map ~f:(fun ((_,patterns),_,_,_) -> List.length patterns) gts
    |> List.max_elt ~compare:Int.compare
    |> Option.value ~default:0
  in
  let mk_general_transition ((start_loc,patterns),cost,(rhss: (OurFloat.t * UpdateElement.t list * string) list),formula) =
    Sequence.of_list (Formula.constraints formula)
    |> Sequence.map ~f:(fun guard ->
        GeneralTransition.mk
          ~start:(Location.of_string start_loc)
          ~fill_up_to_num_arg_vars:number_patterns
          ~patterns:(List.map ~f:Var.of_string patterns)
          ~cost
          (* Create the labels in a delayed fashion to ensure unique ids in the presence of disjunction / multiple constraints*)
          ~rhss:(List.map
                   ~f:(Tuple3.map3 Location.of_string)
                   rhss)
          ~guard)
  in
  Sequence.of_list cleaned_com_k_transitions
  |> Sequence.map ~f:mk_general_transition
  |> Sequence.join
  |> GeneralTransitionSet.of_sequence

let mk_probabilistic_program start general_transitions =
  Program.from_gts start general_transitions
