open! OurBase
open Atoms
open Polynomials

module ConstraintOver
    (M : PolyTypes.Monomial)
    (P : PolyTypes.Polynomial with type monomial = M.t)
    (A : ConstraintTypes.Atom with type polynomial = P.t with type monomial = M.t) =
struct
  module A = A

  type value = A.value
  type polynomial = A.polynomial
  type atom = A.t
  type monomial = M.t
  type monomial_comparator_witness = M.comparator_witness
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

  let non_constant_monomials constr =
    List.map ~f:A.non_constant_monomials constr |> Set.of_list (module M) % List.join


  let monomials_of_atom atom = A.non_constant_monomials atom |> Set.of_list (module M)

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


  let map_var ~subject = List.map ~f:(A.map_var ~subject)
  let map_polynomial f = fold ~subject:f ~le:mk_le ~lt:mk_lt ~correct:mk_true ~conj:mk_and
  let drop_nonlinear constr = List.filter ~f:A.is_linear constr
  let is_linear = List.for_all ~f:A.is_linear

  (**returns a list of the coefficients of a monomial in all the left sides of the constraints*)
  let get_coefficient_vector monomial constr = List.map ~f:(A.get_coefficient monomial) constr

  (**returns a list of the constants of the constraints*)
  let get_constant_vector constr = List.map ~f:A.get_constant constr

  (** returns a list of lists of the coefficients of the constraint*)
  let get_matrix monomials constr = List.map ~f:(fun mon -> get_coefficient_vector mon constr) monomials

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
  include ConstraintOver (Monomials.Make (OurInt)) (Polynomial) (Atom)

  let max_of_occurring_constants atoms =
    atoms |> List.map ~f:Atom.max_of_occurring_constants |> List.fold_left ~f:OurInt.mul ~init:OurInt.one


  let remove_duplicate_atoms = List.dedup_and_sort ~compare:Atom.compare
  let to_set = Set.of_list (module Atom)
  let of_set = Set.to_list
end

module RationalConstraint = struct
  include ConstraintOver (Monomials.Make (OurRational)) (RationalPolynomial) (RationalAtom)

  let max_of_occurring_constants atoms =
    atoms
    |> List.map ~f:RationalAtom.max_of_occurring_constants
    |> List.fold_left ~f:OurRational.mul ~init:OurRational.one


  let of_intconstraint intconstraint =
    mk (List.map ~f:(fun atom -> RationalAtom.of_intatom atom) intconstraint)
end

module ParameterConstraintOver
    (Value : PolyTypes.Ring)
    (Atom : ConstraintTypes.Atom
              with type value = Value.t
               and type polynomial = Value.t Polynomials.generic_var_polynomial
               and type monomial = Monomials.generic_var_monomial)
    (ParamAtom : sig
      include
        ConstraintTypes.Atom
          with type polynomial = Value.t Polynomials.generic_var_polynomial Polynomials.generic_var_polynomial
           and type monomial = Monomials.generic_var_monomial
           and type value = PolynomialOver(Value).t
    end) =
struct
  include
    ConstraintOver (Monomials.Make (PolynomialOver (Value))) (ParameterPolynomialOver (Value)) (ParamAtom)

  module ParaP = ParameterPolynomialOver (Value)
  module C = ConstraintOver (Monomials.Make (Value)) (PolynomialOver (Value)) (Atom)

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

    let non_constant_monomials =
      Set.union (non_constant_monomials constr) (monomials_of_atom param_atom) |> Set.to_list
    in
    let a_matrix = get_matrix non_constant_monomials constr in
    let b_right = get_constant_vector constr in
    let c_left = List.map ~f:(flip A.get_coefficient param_atom) non_constant_monomials in
    let d_right = A.get_constant param_atom in
    apply_farkas a_matrix b_right c_left d_right
end

module ParameterConstraint = ParameterConstraintOver (OurInt) (Atoms.Atom) (Atoms.ParameterAtom)

module RationalParameterConstraint = struct
  include ParameterConstraintOver (OurRational) (Atoms.RationalAtom) (Atoms.RationalParameterAtom)

  let of_intconstraint = of_constraint % RationalConstraint.of_intconstraint
end
