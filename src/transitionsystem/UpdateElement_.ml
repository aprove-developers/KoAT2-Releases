open Batteries
open BoundsInst
open Polynomials

module UpdateValue = struct
  type t = Var of Var.t
         | Dist of ProbabilityDistribution.t [@@deriving eq,ord]

  let to_string ?(pretty=false) ?(to_file=false) = function
    | Dist d -> ProbabilityDistribution.to_string d
    | Var  v -> Var.to_string v

  let rename m = function
    | Dist d -> Dist (ProbabilityDistribution.rename m d)
    | Var  v -> Var (VarIndeterminate.rename m v)

  let vars = function
    | Dist d -> ProbabilityDistribution.vars d
    | Var  v -> VarSet.singleton v

  let of_var v = Var v

  let is_dist = function
    | Dist d -> true
    | Var  v -> false

  let get_var = function
    | Dist d  -> None
    | Var  v  -> Some v

  let exp_value_poly = function
    | Var  v -> RealPolynomial.of_var v
    | Dist d -> ProbabilityDistribution.exp_value_poly d

  let moment_poly d i = match d with
    | Var  v -> RealPolynomial.pow (RealPolynomial.of_var v) i
    | Dist d -> ProbabilityDistribution.moment_poly d i

  let moment_abs_bound d i = match d with
    | Var v  -> RealBound.(pow (of_var v) i)
    | Dist d -> ProbabilityDistribution.moment_abs_bound d i

  let is_integral = fun _ -> true

  let admissibility_constraint = function
    | Var  v -> Guard.mk_true
    | Dist d -> ProbabilityDistribution.admissibility_constraint d

  let as_guard uv new_var = match uv with
    | Var  v -> Guard.Infix.(Polynomial.(of_var v = of_var new_var))
    | Dist d -> ProbabilityDistribution.as_guard d new_var
end

include PolynomialOverIndeterminate(UpdateValue)(OurInt)
module Monomial_ = Monomials.MakeOverIndeterminate(UpdateValue)(OurInt)

let is_integral = fun _ -> true

let to_polynomial =
  fold
    ~const:(Option.some % Polynomial.of_constant)
    ~indeterminate:(Option.map Polynomial.of_var % UpdateValue.get_var)
    ~neg:(Option.map Polynomial.neg)
    ~plus:(OptionMonad.liftM2 Polynomial.add)
    ~times:(OptionMonad.liftM2 Polynomial.mul)
    ~pow:(fun p i -> Option.map (fun p -> Polynomial.pow p i) p)

let of_poly =
  Polynomial.fold
    ~const:of_constant
    ~indeterminate:of_var
    ~neg:neg
    ~plus:add
    ~times:mul
    ~pow:pow

let of_dist d = of_indeterminate (Dist d)

let as_guard ue new_var =
  let (replaced_poly, dist_constrs) =
    fold
      ~const:(fun c -> Polynomial.of_constant c, Guard.mk_true)
      ~indeterminate:(function
          | Var  v -> Polynomial.of_var v, Guard.mk_true
          | Dist d ->
              let temp_v = Var.fresh_id Var.Int () in
              (Polynomial.of_var temp_v, ProbabilityDistribution.as_guard d temp_v)
      )
      ~neg:(Tuple2.map1 Polynomial.neg)
      ~plus:(fun (p1,g1) (p2,g2) -> Polynomial.add p1 p2, Guard.mk_and g1 g2)
      ~times:(fun (p1,g1) (p2,g2) -> Polynomial.mul p1 p2, Guard.mk_and g1 g2)
      ~pow:(fun (p,g) i -> Polynomial.pow p i, g) ue
  in
  Guard.mk_and (Guard.mk_eq (Polynomial.of_var new_var) replaced_poly) dist_constrs

let exp_value_poly: t -> RealPolynomial.t = fun t ->
  List.enum (monomials_with_coeffs t)
  |> Enum.map (Tuple2.map2 (RealPolynomial.product % Enum.map (uncurry UpdateValue.moment_poly) % Monomial_.to_enum))
  |> Enum.map RealPolynomial.(fun(c,p) -> mul (of_intconstant c) p)
  |> RealPolynomial.sum

let moment_poly t i = exp_value_poly (pow t i)

let admissibility_constraint: t -> Guard.t =
  fold
    ~const:(const Guard.mk_true)
    ~indeterminate:(UpdateValue.admissibility_constraint)
    ~neg:identity
    ~plus:Guard.mk_and
    ~times:Guard.mk_and
    ~pow:const

let restore_legacy_distribution_update_semantics v t = match get_indeterminate t with
  | Some (Dist d) -> add (of_var v) t
  | _             -> t

let as_linear_abstract manager constr t new_var =
  let check_if_abs_of_var_is_at_least constr v i =
    let open Formulas.Formula in
    let poly_v = Polynomial.of_var v in
    SMT.Z3Solver.unsatisfiable Infix.(mk constr && poly_v < Polynomial.of_constant i && poly_v > Polynomial.neg (Polynomial.of_constant i))
  in

  let module Multiplicand = struct
    type t = UpdateValue.t * int
    let compare a b = Tuple2.compare ~cmp1:UpdateValue.compare a b
  end in
  let module MultiplicandSet = Set.Make(Multiplicand) in
  let module MultiplicandMap = Map.Make(Multiplicand) in
  let module IndetMap = Map.Make(UpdateValue) in
  let module ClassicalMonomial = Monomials.Make(OurInt) in

  let indeterminates_repr_map = IndetMap.of_enum @@
    Enum.map (fun i -> i, match UpdateValue.get_var i with
                        | Some v -> v
                        | None   -> Var.fresh_id Var.Int ())
      (indeterminates t)
  in
  let indet_guard =
    Enum.map
      (fun(i,v) -> if UpdateValue.is_dist i then UpdateValue.as_guard i v else Guard.mk_true)
      (IndetMap.enum indeterminates_repr_map)
    |> Guard.all % List.of_enum
  in

  let multiplicands_repr_map =
    List.enum (monomials t)
    |> Enum.map Monomial_.to_enum
    |> MultiplicandSet.enum % MultiplicandSet.of_enum % Enum.flatten
    |> Enum.map (fun (i,e) ->
        let v = if e = 1 then IndetMap.find i indeterminates_repr_map else Var.fresh_id Var.Int () in ((i,e),v))
    |> MultiplicandMap.of_enum
  in

  (* post_update refinement constraints for updates of the form X^e where e is even*)
  let multiplicands_post_update_constraint =
    MultiplicandMap.enum multiplicands_repr_map
    |> Enum.filter (fun ((_,e),_) -> e mod 2 = 0 && Int.(e > 0))
    |> Enum.map (fun ((i,e), multipl_repr_var) ->
        let ind_repr_var = IndetMap.find i indeterminates_repr_map in
        let guard = Guard.Infix.(constr && UpdateValue.as_guard i ind_repr_var) in
        let multipl_repr_poly = Polynomial.of_var multipl_repr_var in

        (* check if i^2 is always greater or equal than 4 *)
        if check_if_abs_of_var_is_at_least guard ind_repr_var (OurInt.of_int 2) then
          Guard.Infix.(multipl_repr_poly >= Polynomial.of_int 4 && multipl_repr_poly >= Polynomial.(pow (of_int 2) Int.(e-1) * of_var ind_repr_var))
        (* check if i^2 is always greater or equal than 1 *)
        else if check_if_abs_of_var_is_at_least guard ind_repr_var OurInt.one then
          Guard.Infix.(multipl_repr_poly > Polynomial.zero)
        else
          Guard.Infix.(multipl_repr_poly >= Polynomial.zero)
      )
    |> Guard.all % List.of_enum
  in

  let keep_vars = VarSet.add new_var @@ VarSet.union (Guard.vars constr) (vars t) in
  let all_vars =
    VarSet.union keep_vars (VarSet.union (Guard.vars indet_guard) (Guard.vars multiplicands_post_update_constraint))
  in
  let temp_vars = VarSet.diff all_vars keep_vars in

  let open ApronInterface.Koat2Apron in let open ApronInterface.Apron2Koat in
  let environment = Apron.Environment.make (vars_to_apron all_vars) [||] in

  let post_update_abstract =
    Apron.Abstract1.meet_tcons_array manager (Apron.Abstract1.top manager environment)
      (constraint_to_apron environment multiplicands_post_update_constraint)
  in

  let update_monomial_multiplicands a =
    let vars, assgnmts =
      MultiplicandMap.enum multiplicands_repr_map
      |> Enum.map (fun ((i,e),v) ->
          let ind_repr_var = IndetMap.find i indeterminates_repr_map in
          v, poly_to_apron environment (Polynomial.(pow (of_var ind_repr_var) e))
        )
      |> List.split % List.of_enum
    in
    Apron.Abstract1.assign_texpr_array manager a (Array.map var_to_apron @@ Array.of_list vars) (Array.of_list assgnmts) (Some post_update_abstract)
  in

  let update_final =
    monomials_with_coeffs t
    |> List.map (fun(c,m) -> c,
         Monomial_.to_enum m
         |> Enum.map (fun (i,e) -> MultiplicandMap.find (i,e) multiplicands_repr_map, 1)
         |> ClassicalMonomial.make % List.of_enum
       )
    |> Polynomial.make
  in

  Apron.Abstract1.top manager environment
  |> fun a -> Apron.Abstract1.meet_tcons_array manager a (constraint_to_apron environment constr)
  |> fun a -> Apron.Abstract1.meet_tcons_array manager a (constraint_to_apron environment indet_guard)
  |> update_monomial_multiplicands
  |> fun a -> Apron.Abstract1.assign_texpr manager a (var_to_apron new_var) (poly_to_apron environment update_final) None
  |> fun a -> Apron.Abstract1.forget_array manager a (vars_to_apron temp_vars) false


let as_linear_guard constr t new_var =
  let manager = Ppl.manager_alloc_strict () in
  as_linear_abstract manager constr t new_var
  |> Apron.Abstract1.to_tcons_array manager
  |> ApronInterface.Apron2Koat.constraint_from_apron

let pull_out_of_uniform: t -> t =
  fold
    ~const:of_constant
    ~indeterminate:(function
        | Dist (Uniform (a,b)) ->
          let p,(a',b') = Polynomial.pull_out_common_addends a b in
          add (of_poly p) (of_indeterminate @@ Dist (Uniform (a',b')))
        | i -> of_indeterminate i
      )
    ~neg:neg
    ~plus:add
    ~times:mul
    ~pow:pow

let exp_value_abs_bound t =
  pull_out_of_uniform t
  |> fold
       ~const:(RealBound.of_constant % OurFloat.of_ourint)
       ~indeterminate:(function
           | Dist d -> ProbabilityDistribution.exp_value_abs_bound d
           | Var v -> RealBound.of_var v
         )
       ~neg:identity
       ~plus:RealBound.add
       ~times:RealBound.mul
       ~pow:RealBound.pow

let exp_value_abs_bound t =
  let simplified = pull_out_of_uniform t in
  List.enum (monomials_with_coeffs simplified)
  |> Enum.map
      (Tuple2.map2
         (RealBound.product % Enum.map (uncurry UpdateValue.moment_abs_bound) % Monomial_.to_enum))
  |> Enum.map RealBound.(fun(c,p) -> mul (of_constant @@ OurFloat.of_ourint c) p)
  |> RealBound.sum