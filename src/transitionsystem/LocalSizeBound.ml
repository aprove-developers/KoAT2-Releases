open Batteries

(** A classified bound is a bound of a certain form.
    The different classifications are not disjunctive.
    The upcoming classification set always includes the previous one. *)

type formula = Formula.Make(Polynomials.Make(PolyTypes.OurInt)).t

module Formula = Formula.Make(Polynomials.Make(PolyTypes.OurInt))
module Polynomial = Polynomials.Make(PolyTypes.OurInt)
                  
type classification =
  (** Always smaller or equal to a constant or the value of a prevariable. Examples: x'=x , x'=y , x'=2
      max [c;x1;...;xn]) *)
  | Equality of int
  (** Always smaller or equal to the value of a prevariable plus a constant. Examples: x'=x+1 , x'=y+2 
      d + max [x1;...;xn] *)
  | AddsConstant of int
  (** Always smaller or equal to a scaling factor multiplied with the sum of all prevariables and a constant. Examples: x'=x+y , x'=2*(x+y+z) 
      s * (e + sum [x1;...;xn]) *)
  | ScaledSum of int * int
  (** Always smaller or equal to infinity *)
  | Unbound [@@deriving eq, show]
  
type t = classification * Var.t list [@@deriving eq]

let to_string = function
  | (classification, vars) -> (show_classification classification) ^ " [" ^ (String.concat ", " (List.map Var.to_string vars)) ^ "]"
                
let as_bound classified =
  let open Bound in
  match classified with
  | (Equality c, vars) -> maximum (of_int c :: List.map of_var vars)
  | (AddsConstant d, vars) -> add (of_int d) (maximum (List.map of_var vars))
  | (ScaledSum (s,e), vars) -> mul (of_int s) (add (of_int e) (sum (List.map of_var vars)))
  | (Unbound, _) -> infinity

let as_formula v classified =
  let open Polynomial in
  match classified with
  | (Equality c, vars) ->
     Formula.mk_le_than_max
       (from_var v)
       (of_int c :: List.map from_var vars)
  | (AddsConstant d, vars) ->
     Formula.mk_le_than_max
       (from_var v)
       (List.map (fun v' -> add (from_var v') (of_int d)) vars)
  | (ScaledSum (s,e), vars) ->
     Formula.Infix.(from_var v <= mul (of_int s)
                                 (add (of_int e)
                                      (sum (List.map from_var vars))))
  | (Unbound, _) -> Formula.mk_true

let is_bounded_with var formula classified =
  classified
  |> as_formula var
  |> Formula.implies formula
  |> Formula.neg
  |> SMT.Z3Solver.unsatisfiable

(** Performs a binary search between the lowest and highest value to find the optimal value which satisfies the predicate.
    We assume that the highest value already satisfies the predicate.
    Therefore this method always finds a solution. *)
let rec binary_search lowest highest p =
  if lowest >= highest then
    highest
  else
    let newBound = (lowest + highest) / 2 in
    if p newBound then
      binary_search lowest newBound p
    else
      binary_search (newBound + 1) highest p

let find_equality_bound var formula =
  let low = 0
  and high = 1024 in
  let vars = Set.to_list (Set.remove var (Formula.vars formula)) in
  let is_bound c = is_bounded_with var formula (Equality c, vars) in
  if is_bound high then
    (* TODO If the var is bounded for every constant, we can remove the constant. This is the case e.g. if x'=y *)
    let c = binary_search low high is_bound in
    Some (Equality c, vars)
  else None

let find_addsconstant_bound var formula =
  let low = 0
  and high = 1024 in
  let vars = Set.to_list (Set.remove var (Formula.vars formula)) in
  let is_bound d = is_bounded_with var formula (AddsConstant d, vars) in
  if is_bound high then
    let d = binary_search low high is_bound in
    Some (AddsConstant d, vars)
  else None
                  
let find_bound var formula =
  let finders = List.enum [find_equality_bound; find_addsconstant_bound] in
  try
    Enum.find_map (fun find -> find var formula) finders
  with Not_found -> (Unbound, [])
                  
let sizebound_local kind label var =
  (* If we have an update pattern, it's like x'=b and therefore x'<=b and x >=b and b is a bound for both kinds. *)
  (* TODO Should we also try to substitute vars in the bound if it leads to a simpler bound? E.g. x<=10 && x'=x : b:=x or b:=10? *)
  match TransitionLabel.update label var with
  | Some bound -> Bound.of_poly bound
  | None ->
     match kind with
     (* TODO Use SMT-Solving to find bounds *)
     | TransitionLabel.Upper ->
           label
        |> TransitionLabel.guard
        |> Formula.mk
        |> find_bound var
        |> as_bound
     | TransitionLabel.Lower -> Bound.minus_infinity

