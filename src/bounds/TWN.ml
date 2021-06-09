open Batteries
open Constraints
open Polynomials
open ProgramTypes

(** x_i <- poly(x_j,...) iff. x_i depends on x_j iff. matrix(i,j) iff x_j -> x_i in the order graph*)
let update_matrix (i,j) vars t matrix array set =
    let update_opt = TransitionLabel.update t (List.nth vars i) in
    if i != j && Option.is_some update_opt then
        let update = Option.get update_opt in
        if not (OurInt.is_zero (Polynomial.coeff_of_var (List.nth vars j) update)) then (
            matrix.(i).(j) <- true;
            array.(i) <- array.(i) + 1;
            Set.remove i set)
        else
            set
    else
        set

let rec init (i,j) n vars t matrix array set =
    if j == (n - 1) then
        if i != (n - 1) then
            init (i + 1, 0) n vars t matrix array (update_matrix (i,j) vars t matrix array set)
        else
            set
    else
        init (i, j + 1) n vars t matrix array (update_matrix (i,j) vars t matrix array set)

let rec iterate_set matrix array n (i,j) set =
    if matrix.(i).(j) then (
        matrix.(i).(j) <- false;
        array.(i) <- array.(i) - 1;
    );
    let set_ =
        if array.(i) == 0 then
            Set.add i set
        else
            set in
    if i == (n - 1) then
        set_
    else
        iterate_set matrix array n (i + 1,j) set_


let rec compute_ordering matrix array n set ordering =
    if Set.is_empty set then
        ordering
    else
        (** We take the first element j of the set, delete it and remove all outgoing edges.*)
        let j = Set.any set in
        array.(j) <- -1;
        let set_ = iterate_set matrix array n (0,j) (Set.remove j set) in
        compute_ordering matrix array n set_ (j::ordering)

let check_triangular (t: TransitionLabel.t) =
    let vars = VarSet.to_list (TransitionLabel.vars t) in
    let n = List.length vars in
    let matrix = Array.create_matrix n n false in
    let array = Array.create n 0 in
    let set = init (0,0) n vars t matrix array (Set.of_list (List.range 0 `To (n - 1))) in
    let ordering = List.map (fun i -> List.nth vars i) (List.rev (compute_ordering matrix array n set [])) in
    (* Printf.printf "%S\n" (Util.enum_to_string Var.to_string (List.enum ordering)); *)
    (List.length ordering) == n


let check_twn (l,t,l') =
    (Location.equal l l') && check_triangular t