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
    let from_constant_int c = Constant c
    let from_var_string str = Variable str
    let zero = Constant 0
    let one = Constant 1
    let neg p = Neg p
    let add p1 p2 = Plus (p1, p2)
    let mul p1 p2 = Times (p1, p2)
    let pow p n = Pow (p, n)
    let rec to_string = function
      | Constant c -> string_of_int c
      | Variable v -> v
      | Neg t -> String.concat "" ["("; "-"; to_string t; ")"]
      | Plus (t1,t2) -> String.concat "" ["("; to_string t1; "+"; to_string t2; ")"]
      | Times (t1,t2) -> String.concat "" ["("; to_string t1; "*"; to_string t2; ")"]
      | Pow (p,n) -> String.concat "" ["("; to_string p; "^"; string_of_int n; ")"]
  end

module Atom : Parseable.Atom =
  struct
    module Polynomial_ = Polynomial
    type polynomial = Polynomial_.t
    type t =
      | Equal of polynomial * polynomial
      | Neq of polynomial * polynomial
      | LessThan of polynomial * polynomial
      | LessEqual of polynomial * polynomial
      | GreaterEqual of polynomial * polynomial
      | GreaterThan of polynomial * polynomial
    let mk_gt p1 p2 = GreaterThan(p1, p2)
    let mk_ge p1 p2 = GreaterEqual(p1, p2)
    let mk_lt p1 p2 = LessThan(p1, p2)
    let mk_le p1 p2 = LessEqual(p1, p2)
    let mk_eq p1 p2 = Equal(p1, p2)
    let mk_neq p1 p2 = Neq(p1, p2)
    let to_string c = match c with
      | Equal (p1, p2) -> String.concat " == " [(Polynomial.to_string p1); (Polynomial.to_string p2)]
      | Neq (p1, p2) -> String.concat " <> " [(Polynomial.to_string p1); (Polynomial.to_string p2)]
      | LessThan (p1, p2) -> String.concat " < " [(Polynomial.to_string p1); (Polynomial.to_string p2)]
      | LessEqual (p1, p2) -> String.concat " <= " [(Polynomial.to_string p1); (Polynomial.to_string p2)]
      | GreaterEqual (p1, p2) -> String.concat " >= " [(Polynomial.to_string p1); (Polynomial.to_string p2)]
      | GreaterThan (p1, p2) -> String.concat " > " [(Polynomial.to_string p1); (Polynomial.to_string p2)]
  end

module Constraint : Parseable.Constraint = 
  struct
    module Polynomial_ = Polynomial
    module Atom_= Atom
    type polynomial = Polynomial_.t
    type t = Atom_.t list
      
    let to_string c = String.concat " /\ " ( List.map Atom_.to_string c)
    
    let mk atoms = atoms      
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

    type t = {
        name : string;           
        vars : Constraint_.Atom_.Polynomial_.Var.t list;
        assignments : Constraint_.Atom_.Polynomial_.t list;
        guard : Constraint_.t;
      }

    let mk name vars assignments guard vars =
      { name; vars; assignments; guard }

    let to_string start target transition =
      let varstring = String.concat "," (List.map Constraint_.Atom_.Polynomial_.Var.to_string transition.vars)
      and assignmentstring = String.concat "," (List.map Constraint_.Atom_.Polynomial_.to_string transition.assignments) in
      String.concat "" [start; " ( "; varstring; " ) -> "; transition.name; "( "; assignmentstring; " ) :|: "; Constraint_.to_string transition.guard]
      
  end

module TransitionGraph : Parseable.TransitionGraph =
  struct
    module Transition_ = Transition
    module Location_ = Location

    type t = {
        vars : Transition_.Constraint_.Atom_.Polynomial_.Var.t list;
        edges : (Location_.t * Transition_.t * Location_.t) list;
      }
                     
    let from vars transitions =
      let edges = List.map
                    (fun (start, target, transition) -> (Location_.of_string start, transition, Location_.of_string target))
                    transitions in 
      { vars; edges }

    let to_string graph =
      let varstring = String.concat "" ["( VAR "; String.concat "" (List.map Transition_.Constraint_.Atom_.Polynomial_.Var.to_string graph.vars); " )"]
      and edgestring = String.concat "" ["( RULES \n "; String.concat "\n" (List.map (fun (start, transition, target) -> Transition_.to_string (Location_.to_string start) (Location_.to_string target) transition) graph.edges); " )"] in
      String.concat "\n" [varstring; edgestring]
      
  end
