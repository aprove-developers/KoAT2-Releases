open! OurBase
open Atoms
open Polynomials

module ConstraintOver (P : PolyTypes.Polynomial) (A : ConstraintTypes.Atom with type polynomial = P.t) =
struct
  module A = A

  type value = A.value
  type polynomial = A.polynomial
  type atom = A.t
  type t = A.t list [@@deriving eq, ord]

  let lift atom = [ atom ]
  let mk atoms = atoms
  let mk_true = mk []
  let mk_eq poly1 poly2 = mk A.Infix.[ poly1 <= poly2; poly2 <= poly1 ]
  let mk_gt p1 p2 = lift (A.mk_gt p1 p2)
  let mk_ge p1 p2 = lift (A.mk_ge p1 p2)
  let mk_lt p1 p2 = lift (A.mk_lt p1 p2)
  let mk_le p1 p2 = lift (A.mk_le p1 p2)
  let mk_and = List.append

  module Infix = struct
    let ( = ) = mk_eq
    let ( > ) = mk_gt
    let ( >= ) = mk_ge
    let ( < ) = mk_lt
    let ( <= ) = mk_le
    let ( && ) = mk_and
  end

  let remove_strict = List.map ~f:A.remove_strict

  (** TODO Filter related: x < 0 && x < 1*)
  let all = List.join

  let is_true = function
    | [] -> true
    | _ -> false


  (* TODO Wrong, use SMT-solver here *)
  let ( =~= ) constr1 constr2 =
    List.zip_exn constr1 constr2 |> List.map ~f:(uncurry A.( =~= )) |> List.fold_left ~f:( && ) ~init:true


  let vars constr = constr |> List.map ~f:A.vars |> List.fold_left ~f:Set.union ~init:VarSet.empty

  let to_string ?(to_file = false) ?(pretty = false) ?(conj = " && ") constr =
    String.concat
      ~sep:
        (if pretty then
           " ∧ "
         else
           conj)
      (List.map ~f:(A.to_string ~to_file ~pretty) constr)


  let rename constr varmapping = List.map ~f:(fun atom -> A.rename atom varmapping) constr
  let turn = List.map ~f:A.neg
  let atom_list = identity

  let fold ~subject ~le ~lt ~correct ~conj =
    List.fold_left ~f:(fun c atom -> conj c (A.fold ~subject ~le ~lt atom)) ~init:correct


  let map_polynomial f = fold ~subject:f ~le:mk_le ~lt:mk_lt ~correct:mk_true ~conj:mk_and
  let drop_nonlinear constr = List.filter ~f:A.is_linear constr
  let is_linear = List.for_all ~f:A.is_linear

  (**returns a list of the coefficients of a variable in all the left sides of the constraints*)
  let get_coefficient_vector var constr = List.map ~f:(A.get_coefficient var) constr

  (**returns a list of the constants of the constraints*)
  let get_constant_vector constr = List.map ~f:A.get_constant constr

  (** returns a list of lists of the coefficients of the constraint*)
  let get_matrix vars constr = List.map ~f:(fun var -> get_coefficient_vector var constr) vars

  (** returns a list of lists of the coefficients of the constraint*)
  let dualise vars (matrix : P.t list list) column =
    let dualised_left =
      List.map
        ~f:(fun row -> List.map2_exn ~f:(fun c -> P.mul c % P.of_var) row vars |> Sequence.of_list |> P.sum)
        matrix
    in
    let dualised_eq = List.join (List.map2_exn ~f:mk_eq dualised_left column) in
    let ensure_positivity = List.map ~f:(fun v -> A.Infix.(P.of_var v >= P.zero)) vars in
    mk (List.join [ dualised_eq; ensure_positivity ])
end

module Constraint = struct
  include ConstraintOver (Polynomial) (Atom)

  let max_of_occurring_constants atoms =
    atoms |> List.map ~f:Atom.max_of_occurring_constants |> List.fold_left ~f:OurInt.mul ~init:OurInt.one


  let remove_duplicate_atoms = List.dedup_and_sort ~compare:Atom.compare
  let to_set = Set.of_list (module Atom)
  let of_set = Set.to_list
end

module RationalConstraint = struct
  include ConstraintOver (RationalPolynomial) (RationalAtom)

  let max_of_occurring_constants atoms =
    atoms
    |> List.map ~f:RationalAtom.max_of_occurring_constants
    |> List.fold_left ~f:OurRational.mul ~init:OurRational.one


  let of_intconstraint intconstraint =
    mk (List.map ~f:(fun atom -> RationalAtom.of_intatom atom) intconstraint)
end

module ParameterConstraintOver
    (Value : PolyTypes.Ring)
    (Atom : ConstraintTypes.Atom with type value = Value.t and type polynomial = PolynomialOver(Value).t)
    (ParamAtom : sig
      include
        ConstraintTypes.Atom
          with type polynomial = ParameterPolynomialOver(Value).t
           and type value = PolynomialOver(Value).t
    end) =
struct
  include ConstraintOver (ParameterPolynomialOver (Value)) (ParamAtom)
  module ParaP = ParameterPolynomialOver (Value)
  module C = ConstraintOver (PolynomialOver (Value)) (Atom)

  type unparametrised_constraint = C.t

  let of_constraint =
    C.fold ~subject:(fun p -> ParaP.of_polynomial p) ~le:mk_le ~lt:mk_lt ~correct:mk_true ~conj:mk_and


  (** Farkas Lemma applied to a linear constraint and a cost function given as System Ax<= b, cx<=d. A,b,c,d are the inputs *)
  let apply_farkas a_matrix b_right c_left d_right =
    let num_of_fresh = List.length b_right in
    let fresh_vars = Var.fresh_id_list Var.Real num_of_fresh in
    let dual_constr = C.dualise fresh_vars a_matrix c_left in
    let (const_constr : ParaP.t) = ParaP.of_coeff_list b_right fresh_vars in
    C.Infix.(dual_constr && ParaP.flatten const_constr <= d_right)


  (** Invokes farkas quantifier elimination. Uses apply_farkas*)
  let farkas_transform constr param_atom =
    (* Remove strict comparisons in constraints by replacing them with their non-strict counterparts *)
    let constr, param_atom = (remove_strict constr, A.remove_strict param_atom) in

    let vars = Set.union (vars constr) (A.vars param_atom) |> Set.to_list in
    let a_matrix = get_matrix vars constr in
    let b_right = get_constant_vector constr in
    let c_left = List.map ~f:(flip A.get_coefficient param_atom) vars in
    let d_right = A.get_constant param_atom in
    apply_farkas a_matrix b_right c_left d_right
end

module ParameterConstraint = ParameterConstraintOver (OurInt) (Atoms.Atom) (Atoms.ParameterAtom)

module RationalParameterConstraint = struct
  include ParameterConstraintOver (OurRational) (Atoms.RationalAtom) (Atoms.RationalParameterAtom)

  let of_intconstraint = of_constraint % RationalConstraint.of_intconstraint
end
