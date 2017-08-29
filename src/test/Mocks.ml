module Polynomial : Parseable.Polynomial =
  struct
    module Var = ID.StringID
    type t =
      | Constant of int
      | Variable of string
      | Neg of t
      | Plus of t * t
      | Times of t * t
      | Pow of t * int              
    let value c = Constant c
    let var str = Variable str
    let zero = Constant 0
    let one = Constant 1
    let neg p = Neg p
    let add p1 p2 = Plus (p1, p2)
    let mul p1 p2 = Times (p1, p2)
    let pow p n = Pow (p, n)
    let rec to_string = function
      | Constant c -> string_of_int c
      | Variable v -> v
      | Neg t -> "(-" ^ to_string t ^ ")"
      | Plus (t1,t2) -> "(" ^ to_string t1 ^ "+" ^ to_string t2 ^ ")"
      | Times (t1,t2) -> "(" ^ to_string t1 ^ "*" ^ to_string t2 ^ ")"
      | Pow (p,n) -> "(" ^ to_string p ^ "^" ^ string_of_int n ^ ")"
  end

module Atom : Parseable.Atom =
  struct
    module Polynomial_ = Polynomial
    type polynomial = Polynomial_.t
    type t =
      | LessThan of polynomial * polynomial
      | LessEqual of polynomial * polynomial
      | GreaterEqual of polynomial * polynomial
      | GreaterThan of polynomial * polynomial
    let mk_gt p1 p2 = GreaterThan(p1, p2)
    let mk_ge p1 p2 = GreaterEqual(p1, p2)
    let mk_lt p1 p2 = LessThan(p1, p2)
    let mk_le p1 p2 = LessEqual(p1, p2)
    let to_string c = match c with
      | LessThan (p1, p2) -> Polynomial.to_string p1 ^ " < " ^ Polynomial.to_string p2
      | LessEqual (p1, p2) -> Polynomial.to_string p1 ^ " <= " ^ Polynomial.to_string p2
      | GreaterEqual (p1, p2) -> Polynomial.to_string p1 ^ " >= " ^ Polynomial.to_string p2
      | GreaterThan (p1, p2) -> Polynomial.to_string p1 ^ " > " ^ Polynomial.to_string p2
  end

module Constraint : Parseable.Constraint = 
  struct
    module Polynomial_ = Polynomial
    module Atom_= Atom
    type polynomial = Polynomial_.t
    type t = Atom_.t list
      
    let to_string c = String.concat " /\ " ( List.map Atom_.to_string c)
    
    let mk atoms = atoms
    let mk_eq poly1 poly2 =
      mk [Atom_.mk_le poly1 poly2; Atom_.mk_le poly2 poly1]
    let mk_gt p1 p2 = mk [Atom_.mk_gt p1 p2]
    let mk_ge p1 p2 = mk [Atom_.mk_ge p1 p2]
    let mk_lt p1 p2 = mk [Atom_.mk_lt p1 p2]
    let mk_le p1 p2 = mk [Atom_.mk_le p1 p2]
    let all = List.flatten 

    let is_true = function
      | [] -> true
      | _ -> false
  end

module Location : Parseable.Location =
  struct
    type t = string
    let to_string l = l
    let of_string l = l
  end
   
module Transition : Parseable.Transition =
  struct
    module Constraint_ = Constraint

    exception RecursionNotSupported

    type t = {
        name : string;
        start : string;
        target : string;
        vars : Constraint_.Atom_.Polynomial_.Var.t list;
        assignments : Constraint_.Atom_.Polynomial_.t list;
        guard : Constraint_.t;
      }

    let mk ~name ~start ~targets ~patterns ~guard ~vars =
      if List.length targets != 1 then raise RecursionNotSupported else
        let (target, assignments) = List.hd targets in
        { name; start; target; vars; assignments; guard }

    let start t = t.start

    let target t = t.target

    let to_string start target transition =
      let varstring = String.concat "," (List.map Constraint_.Atom_.Polynomial_.Var.to_string transition.vars)
      and assignmentstring = String.concat "," (List.map Constraint_.Atom_.Polynomial_.to_string transition.assignments) in
      let without_guard = [start; "("; varstring; ")"; "->"; transition.name; "("; target; "("; assignmentstring; ")"; ")"] in
      let transition_elements = if Constraint_.is_true transition.guard
                                then without_guard
                                else List.append without_guard [":|:"; Constraint_.to_string transition.guard] in
      String.concat " " transition_elements
      
  end

module TransitionGraph : Parseable.TransitionGraph =
  struct
    module Transition_ = Transition
    module Location_ = Location

    type t = {
        vars : Transition_.Constraint_.Atom_.Polynomial_.Var.t list;
        edges : (Location_.t * Transition_.t * Location_.t) list;
        start : Location_.t;
      }
                     
    let from vars transitions start =
      let edges = List.map (fun t -> (Location_.of_string (Transition_.start t),
                                      t,
                                      Location_.of_string (Transition_.target t)))
                           transitions in
      { vars; edges; start }

    let to_string graph =
      let goalstring = String.concat " " ["("; "GOAL"; "COMPLEXITY"; ")"]
      and startterm = String.concat " " ["("; "STARTTERM"; "("; "FUNCTIONSYMBOLS"; Location_.to_string graph.start; ")"; ")"]
      and varstring = String.concat " " ["("; "VAR"; String.concat "" (List.map Transition_.Constraint_.Atom_.Polynomial_.Var.to_string graph.vars); ")"]
      and edgestring = String.concat " " ["("; "RULES"; "\n"; String.concat "\n" (List.map (fun (start, transition, target) -> Transition_.to_string (Location_.to_string start) (Location_.to_string target) transition) graph.edges); ")"] in
      String.concat "\n" [goalstring; startterm; varstring; edgestring]
      
  end
