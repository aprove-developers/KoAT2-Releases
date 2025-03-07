open! OurBase
open Polynomials

let logger = Logging.(get Bound)

module Make (Num : PolyTypes.OurNumber) = struct
  type valuation = Valuation.Make(Num).t
  type polynomial = PolynomialOver(Num).t
  type value = Num.t
  type indeterminate = Var.t

  module Poly = PolynomialOver (Num)

  (* Minus Infinity is max of an empty list *)
  (* Infinity is min of an empty list *)
  type bound =
    | Const of Num.t (* absolute value *)
    | Var of Var.t
    | Exp of bound * bound
    | Sum of bound * bound
    | Product of bound * bound
    | Log of Var.t
      (* We always consider log-Base 2. And we always normalize s.t. log(x^2 + 2^x) <= 2*log(x) + x. *)
  [@@deriving eq, ord]

  type t = bound option [@@deriving eq, ord]
  (* None represents infinity *)

  let prove_finiteness = identity
  let bound_of_var v = Var v
  let of_var = OptionMonad.return % bound_of_var
  let bound_of_constant c = Const (Num.abs c)
  let of_constant = OptionMonad.return % bound_of_constant
  let of_OurInt = OptionMonad.return % bound_of_constant % Num.of_ourint

  let is_constant =
    let rec is_constant = function
      | Const _ -> true
      | Var _ -> false
      | Exp (_, b) -> is_constant b
      | Sum (b1, b2) -> is_constant b1 && is_constant b2
      | Product (b1, b2) -> is_constant b1 && is_constant b2
      | Log b -> false
    in
    Option.value ~default:false % Option.map ~f:is_constant


  let rec fold_bound ~const ~var ~plus ~times ~exp ~log p =
    let fold_ = fold_bound ~const ~var ~plus ~times ~exp ~log in
    match p with
    | Var v -> var v
    | Const c -> const c
    | Exp (b, e) -> exp (fold_ b) (fold_ e)
    | Sum (b1, b2) -> plus (fold_ b1) (fold_ b2)
    | Product (b1, b2) -> times (fold_ b1) (fold_ b2)
    | Log v -> log v


  let fold ~const ~var ~plus ~times ~exp ~log ~inf = function
    | Some b -> fold_bound ~const ~var ~plus ~times ~exp ~log b
    | None -> inf


  let to_poly_exp_helper bo eo =
    let open OptionMonad in
    let* b = bo in
    let* e = eo in
    if Poly.is_const e then
      pure (Poly.pow b @@ Num.to_int @@ Poly.get_constant e)
    else
      None


  let to_poly =
    fold ~const:(Option.some % Poly.of_constant) ~var:(Option.some % Poly.of_var)
      ~plus:(OptionMonad.liftM2 Poly.add) ~times:(OptionMonad.liftM2 Poly.mul) ~exp:to_poly_exp_helper
      ~log:(fun _ -> None)
      ~inf:None


  let to_poly_overappr_logs =
    fold ~const:(Option.some % Poly.of_constant) ~var:(Option.some % Poly.of_var)
      ~plus:(OptionMonad.liftM2 Poly.add) ~times:(OptionMonad.liftM2 Poly.mul) ~exp:to_poly_exp_helper
      ~log:(Option.some % Poly.of_var) ~inf:None


  (** Bound is in corresponding asymptotic class O(2^2^...^n) where the integer value denotes the amount of powers.*)
  type complexity =
    | LogarithmicPolynomial of OurRational.t * OurRational.t
        (** Bound is in asymptotic class O(log(n)^i * n^j) *)
    | Exponential of int
    | Inf  (** Bound is infinite. *)
  [@@deriving eq]

  let correct_str base_str x =
    base_str
    ^
    if OurRational.(equal x one) then
      ""
    else
      "^" ^ OurRational.to_string x


  let rec show_complexity b =
    match b with
    | Inf -> "Infinity"
    | LogarithmicPolynomial (x, y) when OurRational.(equal x zero && equal y zero) -> "O(1)"
    | LogarithmicPolynomial (x, y) when OurRational.(equal x zero) -> "O(" ^ correct_str "n" y ^ ")"
    | LogarithmicPolynomial (x, y) when OurRational.(equal y zero) -> "O(" ^ correct_str "log(n)" x ^ ")"
    | LogarithmicPolynomial (x, y) -> "O(" ^ correct_str "log(n)" x ^ "*" ^ correct_str "n" y ^ ")"
    | Exponential 1 -> "O(EXP)"
    | Exponential x -> "O(EXP^" ^ show_complexity (Exponential (x - 1)) ^ ")"


  let show_complexity_termcomp = function
    | Inf -> "MAYBE"
    | LogarithmicPolynomial (x, y) when OurRational.(equal x zero && equal y zero) -> "WORST_CASE(?, O(1))"
    | LogarithmicPolynomial (x, y) -> "WORST_CASE(?, O(n^" ^ OurRational.(to_string (x + y)) ^ "))"
    | Exponential _ -> "WORST_CASE(?, O(EXP))"


  let show_complexity_termcomp_log = function
    | Inf -> "MAYBE"
    | LogarithmicPolynomial (x, y) when OurRational.(equal x zero && equal y zero) -> "WORST_CASE(?, O(1))"
    | LogarithmicPolynomial (x, y) when OurRational.(equal x zero) ->
        "WORST_CASE(?, O(" ^ correct_str "n" y ^ "))"
    | LogarithmicPolynomial (x, y) when OurRational.(equal y zero) ->
        "WORST_CASE(?, O(" ^ correct_str "log(n)" x ^ "))"
    | LogarithmicPolynomial (x, y) ->
        "WORST_CASE(?, O(" ^ correct_str "log(n)" x ^ "*" ^ correct_str "n" y ^ "))"
    | Exponential _ -> "WORST_CASE(?, O(EXP))"


  let lex_compare (b1, a1) (b2, a2) =
    if OurRational.equal b1 b2 && OurRational.equal a1 a2 then
      0
    else if OurRational.(b1 > b2) || OurRational.(b1 = b2 && a1 > a2) then
      1
    else
      -1


  let asymptotic_complexity =
    let rec go = function
      | Const c -> LogarithmicPolynomial (OurRational.zero, OurRational.zero)
      | Var v -> LogarithmicPolynomial (OurRational.zero, OurRational.one)
      | Sum (x, y) -> (
          match (go x, go y) with
          | Inf, _ -> Inf
          | _, Inf -> Inf
          | LogarithmicPolynomial (x1, y1), LogarithmicPolynomial (x2, y2) ->
              if lex_compare (y1, x1) (y2, x2) == -1 then
                LogarithmicPolynomial (x2, y2)
              else
                LogarithmicPolynomial (x1, y1)
          | Exponential x, Exponential y -> Exponential (Int.max x y)
          | LogarithmicPolynomial _, Exponential y -> Exponential y
          | Exponential x, LogarithmicPolynomial _ -> Exponential x)
      | Product (x, y) -> (
          match (go x, go y) with
          | Inf, _ -> Inf
          | _, Inf -> Inf
          | LogarithmicPolynomial (x1, y1), LogarithmicPolynomial (x2, y2) ->
              LogarithmicPolynomial (OurRational.(x1 + x2), OurRational.(y1 + y2))
          | Exponential x, Exponential y -> Exponential (Int.max x y)
          | LogarithmicPolynomial _, Exponential y -> Exponential y
          | Exponential x, LogarithmicPolynomial _ -> Exponential x)
      | Exp (b, Const c) -> (
          let c_our_rational = Num.to_our_rational c in
          match go b with
          | LogarithmicPolynomial (x, y) ->
              LogarithmicPolynomial (OurRational.(x * c_our_rational), OurRational.(y * c_our_rational))
          | Exponential _ as e -> e
          | Inf -> Inf)
      | Exp (b, e) -> (
          match go e with
          | Inf -> Inf
          | LogarithmicPolynomial _ -> Exponential 1
          | Exponential x -> Exponential (Int.succ x))
      | Log _ -> LogarithmicPolynomial (OurRational.one, OurRational.zero)
    in
    Option.value_map ~f:go ~default:Inf


  (** Returns true iff. the bound is in complexity class O(n) *)
  let is_linear bound =
    match asymptotic_complexity bound with
    | LogarithmicPolynomial (_, y) when OurRational.(equal y zero) -> true
    | LogarithmicPolynomial (_, y) when OurRational.(equal y one) -> true
    | _ -> false


  let is_polynomial bound =
    match asymptotic_complexity bound with
    | LogarithmicPolynomial _ -> true
    | _ -> false


  let compare_asy b1 b2 =
    match (asymptotic_complexity b1, asymptotic_complexity b2) with
    | Inf, Inf -> 0
    | Inf, _ -> 1
    | _, Inf -> -1
    | Exponential x, Exponential y -> Int.compare x y
    | Exponential x, _ -> 1
    | _, Exponential y -> -1
    | LogarithmicPolynomial (x1, y1), LogarithmicPolynomial (x2, y2) -> lex_compare (y1, x1) (y2, x2)


  let min_asy b1 b2 =
    if compare_asy b1 b2 <= 0 then
      b1
    else
      b2


  let max_of_occurring_constants bound =
    fold ~const:identity
      ~var:(fun _ -> Num.one)
      ~plus:Num.add ~times:Num.mul
      ~exp:(fun _ -> raise (Failure "Can not compute max_of_occurring_constants for non-polynomial bounds!"))
      ~log:(fun _ -> Num.one)
      ~inf:(raise (Failure "Can not compute max_of_occurring_constants for non-polynomial bounds!"))
      bound


  let get_op_chain_and_apply_to_atoms f t =
    let rec go b =
      match (t, b) with
      | `Sum, Sum (b1, b2) -> go b1 @ go b2
      | `Product, Product (b1, b2) -> go b1 @ go b2
      | _, b -> (
          match (t, f b) with
          | `Sum, Sum (b1, b2) -> go b1 @ go b2
          | `Product, Product (b1, b2) -> go b1 @ go b2
          | _, b -> [ f b ])
    in
    go


  let rec show_bound_inner ?(pretty = false) =
    let mul_sign =
      if pretty then
        "⋅"
      else
        "*"
    in
    function
    | Var v -> Var.to_string ~pretty v
    | Log v -> "log(" ^ Var.to_string ~pretty v ^ ")"
    | Const c -> Num.to_string c
    | Exp (b, e) ->
        let to_string_ t =
          match t with
          | Var v -> Var.to_string ~pretty v
          | Const c -> Num.to_string c
          | b -> "(" ^ show_bound_inner ~pretty b ^ ")"
        in
        to_string_ b ^ "^" ^ to_string_ e
    | Sum (_, _) as b ->
        (* Order terms by degree*)
        let sum_chain = get_op_chain_and_apply_to_atoms identity `Sum b in
        let sorted_chain =
          List.sort
            ~compare:(fun b1 b2 -> compare_asy (OptionMonad.return b2) (OptionMonad.return b1))
            sum_chain
        in
        List.foldi
          ~f:(fun i s b ->
            if i = 0 then
              show_bound_inner ~pretty b
            else
              s ^ "+" ^ show_bound_inner ~pretty b)
          ~init:"" sorted_chain
    | Product (Sum (b1, b2), Sum (b3, b4)) ->
        "("
        ^ show_bound_inner ~pretty (Sum (b1, b2))
        ^ ")" ^ mul_sign ^ "("
        ^ show_bound_inner ~pretty (Sum (b3, b4))
        ^ ")"
    | Product (Sum (b1, b2), b3) ->
        "(" ^ show_bound_inner ~pretty (Sum (b1, b2)) ^ ")" ^ mul_sign ^ show_bound_inner ~pretty b3
    | Product (b1, Sum (b2, b3)) ->
        show_bound_inner ~pretty b1 ^ mul_sign ^ "(" ^ show_bound_inner ~pretty (Sum (b2, b3)) ^ ")"
    | Product (b1, b2) -> show_bound_inner ~pretty b1 ^ mul_sign ^ show_bound_inner ~pretty b2


  let show_bound ?(pretty = false) t =
    match t with
    | Some b -> show_bound_inner ~pretty b
    | None -> "inf"


  let show ?(pretty = false) ?(complexity = true) ?(termination_only = false) bound =
    if termination_only then
      if Option.is_some bound then
        "YES"
      else
        "MAYBE"
    else
      let complexity_str =
        if complexity then
          " {" ^ (show_complexity % asymptotic_complexity) bound ^ "}"
        else
          ""
      in
      show_bound ~pretty bound ^ complexity_str


  let to_string ?(pretty = false) ?(termination_only = false) =
    show ~complexity:true ~pretty ~termination_only


  let show_finiteness bound =
    if Option.is_none bound then
      "infinite"
    else
      "finite"


  let gt_bound b1 b2 =
    let execute () =
      match (b1, b2) with
      | Const c1, Const c2 when Num.Compare.(c1 > c2) -> Some true
      | _ -> None
    in
    Logger.with_log logger Logger.DEBUG
      (fun () -> (">", [ ("condition", to_string (Some b1) ^ ">" ^ to_string (Some b2)) ]))
      ~result:(Util.option_to_string Bool.to_string)
      execute


  let ( > ) b1 b2 =
    match (b1, b2) with
    | Some b1, Some b2 -> gt_bound b1 b2
    | None, Some _ -> Some true
    | _, None -> Some false


  let ( >= ) b1 b2 =
    let execute () =
      match (b1, b2) with
      | None, None -> Some true
      | Some b1, Some b2 -> (
          if equal_bound b1 b2 then
            Some true
          else
            match (b1, b2) with
            | Const c1, Const c2 when Num.Compare.(c1 >= c2) -> Some true
            | _ -> None)
      | _ -> Some false
    in
    Logger.with_log logger Logger.DEBUG
      (fun () -> (">=", [ ("condition", to_string b1 ^ ">=" ^ to_string b2) ]))
      ~result:(Util.option_to_string Bool.to_string)
      execute


  let ( < ) = flip ( > )
  let ( <= ) = flip ( >= )
  let ( =~= ) = equal

  (* In the following we simplify (finite) bounds to normal forms.
   * A bound b is in normal form if it can be represented as the sum of products of coefficients and variables, or terms n^b where b
   * is in normal form.
   * E.g. (x+y)*(x+y) should be simplified to x^2 + 2*x*y + y^2
   *)
  let rec get_op_chain t = get_op_chain_and_apply_to_atoms simplify_bound t

  (* Reverse to get_op_chain, e.g. construct a chain *)
  and construct_op_chain t bs =
    (* Sort terms to allow for better equality checking of similar terms. String comparison is kind of arbitrary *)
    (* We sort in reverse order, and then apply in reverse order again.
     * This has the following advantage:
           Consider a product chain [Var X, Const 1, Var Y]
           Then, by sorting in reverse we get [Var Y, Var X, Const 1].
           By, again applying in reverse we obtain Product (Var X, Var Y) -> Product (Const 1, Product (Var X, Var Y)).
           The coefficient up-front then allows for better simplification later-on
     * *)
    let sorted =
      List.sort ~compare:(fun b1 b2 -> String.compare (show_bound_inner b2) (show_bound_inner b1)) bs
    in
    match t with
    | `Sum -> (
        try List.reduce_exn ~f:(fun b1 b2 -> Sum (b2, b1)) sorted with
        | Invalid_argument _ -> bound_of_constant Num.zero)
    | `Product -> (
        try List.reduce_exn ~f:(fun b1 b2 -> Product (b2, b1)) sorted with
        | Invalid_argument _ -> bound_of_constant Num.one)


  and simplify_bound bound =
    let execute () =
      match bound with
      | Var v -> Var v
      | Log v -> Log v
      | Const c -> Const c
      (* Simplify terms with sum head *)
      | Sum (_, _) as b ->
          let sum_chain = get_op_chain `Sum b |> List.filter ~f:(not % equal_bound (Const Num.zero)) in
          (* Merge addends that are a product of the same bound with different coefficients *)
          let combine_chain_elements_with_coeffs =
            let get_coeff_elem = function
              | Product (Const c, b) -> (b, c)
              | Product (b, Const c) -> (b, c)
              | Const k -> (Const Num.one, k)
              | b -> (b, Num.one)
            in
            sum_chain |> List.map ~f:get_coeff_elem
            |> List.fold_left
                 ~f:(fun l (b, c) ->
                   match List.findi ~f:(fun _ -> equal_bound b % fst) l with
                   | Some (i, _) -> List.modify_at i ~f:(fun (b, c') -> (b, Num.(c + c'))) l
                   | None -> List.cons (b, c) l)
                 ~init:[]
            |> List.map ~f:(fun (b, c) -> simplify_bound @@ Product (Const c, b))
          in
          (* Finally take the chain with possibly merged addends and construct a bound before applying the 'old' approach to it *)
          construct_op_chain `Sum combine_chain_elements_with_coeffs
      (* Simplify terms with product head *)
      | Product (_, _) as b ->
          let chain = get_op_chain `Product b in

          (* Partition the chain in sets of constants and non-constant factors *)
          let all_consts, all_non_consts =
            List.partition_map chain ~f:(function
              | Const c -> Base__Either0.First c
              | b -> Base__Either0.Second b)
          in

          (* Get the coefficient of the complete chain by multiplying all of its constant values *)
          let const = List.fold_left ~f:Num.mul ~init:Num.one all_consts in

          let group_exps product_chain =
            let exps_wo_const_log, const_logs_exps_and_non_exps =
              List.partition_map product_chain ~f:(function
                | Exp (Const _, Log _) as b -> Base__.Either0.Second b
                | Exp (b, e) -> Base__.Either0.First (b, e)
                | Var _ as b -> Base__.Either0.First (b, Const Num.one)
                | b -> Base__.Either0.Second b)
            in
            let back_to_bound (b, e) =
              match (simplify_bound b, simplify_bound e) with
              | (Const _ as b), (Const _ as e) -> simplify_bound (Exp (b, e))
              | _, Const c when Num.(equal c one) -> b
              | b, e -> Exp (b, e)
            in
            let exps_transformed =
              List.fold_left exps_wo_const_log ~init:[] ~f:(fun acc_list (b, e) ->
                  match List.findi acc_list ~f:(fun _ (b', e') -> equal_bound b b' || equal_bound e e') with
                  | Some (i, (b', _)) when equal_bound b b' ->
                      List.modify_at i acc_list ~f:(fun (b, eprev) -> (b, Sum (eprev, e)))
                  | Some (i, (_, (Const c as e'))) when equal_bound e e' && Num.(c <> one) ->
                      (* Here, we only progress with simplification if exponent is not equal to 1, so check for this *)
                      List.modify_at i acc_list ~f:(fun (bprev, e) -> (Product (bprev, b), e))
                  | _ -> List.cons (b, e) acc_list)
              |> List.map ~f:back_to_bound
            in
            exps_transformed @ const_logs_exps_and_non_exps
          in

          let final_sum_chain multiply_with_const =
            (* Here we expect all_non_consts to be sums or vars, or exponential terms.
             * For all sums we have to multiply the corresponding chains *)
            List.map ~f:(get_op_chain `Sum) all_non_consts
            (* multiply through *)
            |> List.Cartesian_product.all
            |> List.map ~f:(fun l ->
                   if multiply_with_const then
                     Const const :: l
                   else
                     l)
            |> List.map ~f:group_exps
            |> List.map ~f:(construct_op_chain `Product)
            |> fun l ->
            if Int.(List.length l > 1) then
              simplify_bound (construct_op_chain `Sum l)
            else
              construct_op_chain `Sum l
          in

          if Num.(equal const zero) then
            Const const
          else if Num.(equal const one) then
            final_sum_chain false
          else if List.is_empty all_non_consts then
            Const const
          else
            final_sum_chain true
      (* Simplify terms with exp head *)
      | Exp (base, exponent) -> (
          match (simplify_bound base, simplify_bound exponent) with
          | Const value, _ when Num.(equal value zero) -> Const Num.zero
          | Const value, _ when Num.(equal value one) -> Const Num.one
          | Const value, Const e ->
              Option.map (Num.root_pow value e) ~f:(fun r -> Const r) |? Exp (base, exponent)
          | b, Const e when Num.is_integral e ->
              (* TODO Do not use Num.to_int *)
              Util.iterate_n_times (fun b' -> Product (b, b')) (Num.to_int e) (Const Num.one)
              |> simplify_bound
          | Const c, (Sum (_, _) as e) ->
              let logs, non_logs =
                get_op_chain `Sum e
                |> List.partition_map ~f:(function
                     | Log v -> Base__Either0.First (`Log v)
                     | b -> Base__Either0.Second b)
              in
              let exp_logs_transformed = List.map logs ~f:(fun (`Log v) -> Exp (Const c, Log v)) in
              let exp_non_logs_transformed = Exp (Const c, construct_op_chain `Sum non_logs) in
              construct_op_chain `Product (exp_non_logs_transformed :: exp_logs_transformed)
              |> ite (List.is_empty logs) identity simplify_bound
          | Const b, Log v -> Product (Const (Num.log b), bound_of_var v)
          | base, exponent -> Exp (base, exponent))
    in

    Logger.with_log logger Logger.DEBUG
      (fun () -> ("simplify_bound", [ ("input", to_string (Some bound)) ]))
      ~result:(to_string % OptionMonad.return) execute


  let simplify = Option.map ~f:simplify_bound
  let zero_bound = Const Num.zero
  let zero = Some zero_bound
  let one_bound = Const Num.one
  let one = Some one_bound
  let add_bound b1 b2 = simplify_bound (Sum (b1, b2))
  let add = OptionMonad.liftM2 add_bound
  let mul_bound b1 b2 = simplify_bound (Product (b1, b2))

  let max b1 b2 =
    match (b1, b2) with
    | Some (Const c1), Some (Const c2) -> Option.return @@ Const (Num.max c1 c2)
    | b1, b2 -> add b1 b2 (* TODO Improve this. *)


  let maximum bounds = Sequence.reduce ~f:max bounds |> Option.map ~f:simplify |? zero

  let mul b1 b2 =
    match (b1, b2) with
    (* 0 * inf = inf * 0 = 0 *)
    | Some (Const c), None ->
        if Num.equal c Num.zero then
          zero
        else
          None
    | None, Some (Const c) ->
        if Num.equal c Num.zero then
          zero
        else
          None
    | _ -> OptionMonad.liftM2 mul_bound b1 b2


  let pow_bound bound n = Util.iterate_n_times (mul_bound bound) n one_bound |> simplify_bound
  let pow bound n = Option.map ~f:(flip pow_bound n) bound

  (** Addition of two elements. *)
  let ( + ) = add

  (** Multiplication of two elements *)
  let ( * ) = mul

  (** Raises an element to the power of an integer value. *)
  let ( ** ) = pow

  let exp_bound b e = simplify_bound (Exp (b, e))
  let exp = OptionMonad.liftM2 exp_bound

  (* simplify @@ Option.map ~f:(exp_bound value) b *)
  let exp_int value b = Option.map ~f:((fun c -> exp_bound @@ Const (Num.of_ourint c)) value) b
  let infinity = None
  let sum bounds = Sequence.reduce ~f:add bounds |> Option.map ~f:simplify |? zero
  let sum_list = sum % Sequence.of_list
  let product bounds = Sequence.reduce ~f:mul bounds |> Option.map ~f:simplify |? one

  let of_poly =
    OptionMonad.return
    % Poly.fold ~const:bound_of_constant ~indeterminate:bound_of_var ~plus:add_bound ~times:mul_bound
        ~pow:pow_bound


  let of_intpoly =
    OptionMonad.return
    % Polynomial.fold ~const:(bound_of_constant % Num.of_ourint) ~indeterminate:bound_of_var ~plus:add_bound
        ~times:mul_bound ~pow:pow_bound


  let log v = Option.return (Log v)
  let log_of_constant = of_constant % Num.log

  let log_of_bound =
    let rec go = function
      | Const c -> log_of_constant c
      | Var v -> log v
      | Sum (b1, b2) -> add (go b1) (go b2)
      | Product (b1, b2) -> add (go b1) (go b2)
      | Exp (b, e) -> mul (go b) (Option.some e)
      | Log _ as l -> Option.some l
    in
    Option.bind ~f:go


  let log_of_poly p =
    let of_poly = of_intpoly p in
    log_of_bound of_poly


  let bound_of_int i = Const (Num.of_int @@ Int.abs i)
  let of_int = OptionMonad.return % bound_of_int
  let bound_of_var_string str = Var (Var.of_string str)
  let of_var_string = OptionMonad.return % bound_of_var_string
  let is_infinity = Option.is_none
  let is_finite = Option.is_some

  let substitute_f (substitution : Var.t -> t) (bound : t) : t =
    OptionMonad.(
      bound
      >>= fold_bound ~const:of_constant ~var:substitution ~plus:add ~times:mul ~exp
            ~log:(log_of_bound % substitution))


  let substitute_bound var ~replacement =
    substitute_f (fun target_var ->
        if Var.(var =~= target_var) then
          replacement
        else
          of_var target_var)


  let substitute var ~replacement b = substitute_bound var ~replacement b

  let substitute_all substitution =
    substitute_f (fun var -> Map.find_default ~default:(of_var var) substitution var)


  let rec vars_bound = function
    | Var v -> VarSet.singleton v
    | Log v -> VarSet.singleton v
    | Const _ -> VarSet.empty
    | Exp (v, b) -> vars_bound b
    | Sum (b1, b2) -> Set.union (vars_bound b1) (vars_bound b2)
    | Product (b1, b2) -> Set.union (vars_bound b1) (vars_bound b2)


  let vars = Option.value ~default:VarSet.empty % Option.map ~f:vars_bound
  let indeterminates = Set.to_list % vars

  let keep_simpler_bound b1 b2 =
    match compare_asy b1 b2 with
    (* First compare asymptotic_complexity *)
    | -1 -> b1
    | 1 -> b2
    | _ -> (
        (* Now compare number of variables*)
        match Int.compare (Set.length @@ vars b1) (Set.length @@ vars b2) with
        | -1 -> b1
        | 1 -> b2
        | _ -> (
            (* Finally compare length of to_string *)
            match Int.compare (String.length @@ show_bound b1) (String.length @@ show_bound b2) with
            | -1 -> b1
            | 1 -> b2
            | _ -> b1))


  let degree n = raise (Failure "degree for MinMaxPolynomial not yet implemented")
  let rename map p = raise (Failure "rename for MinMaxPolynomial not yet implemented")
  let eval p valuation = raise (Failure "eval for MinMaxPolynomial not yet implemented")
  let eval_f p valuation = raise (Failure "eval_f for MinMaxPolynomial not yet implemented")
  let coeff_of_var p = raise (Failure "coeff_of_var for MinMaxPolynomial not yet implemented")
  let of_coeff_list p = raise (Failure "of_coeff_list for MinMaxPolynomial not yet implemented")
end

module Bound = Make (OurInt)

module RationalBound = struct
  include Make (OurRational)

  let of_intbound =
    Bound.fold
      ~const:(of_constant % OurRational.of_ourint)
      ~var:of_var ~plus:add ~times:mul
      ~exp:(fun b -> exp b)
      ~log ~inf:infinity


  let of_overapprox_laurentpoly =
    RationalPolynomial.fold ~const:of_constant ~indeterminate:of_var ~plus:add ~times:mul ~pow
    % RationalLaurentPolynomial.overapprox_neg_exponents


  let to_intbound =
    (* TODO Move this to Make *)
    fold
      ~const:(Bound.of_constant % OurRational.ceil)
      ~var:Bound.of_var ~plus:Bound.add ~times:Bound.mul ~exp:Bound.exp ~log:Bound.of_var ~inf:infinity


  let mth_root m b = exp b (of_constant @@ OurRational.make OurInt.one m)
  let sqrt = mth_root (OurInt.of_int 2)
end

module BinaryBound = struct
  type t = Finite | Infinite [@@deriving eq, ord]
  type bound = Bound
  type value
  type polynomial
  type valuation
  type indeterminate

  let prove_finiteness = function
    | Finite -> Some Bound
    | Infinite -> None


  let of_poly _ = Finite
  let of_intpoly _ = Finite
  let to_poly _ = None
  let to_poly_overappr_logs _ = None
  let of_constant _ = Finite
  let of_OurInt _ = Finite

  let is_constant = function
    | Finite -> true
    | Infinite -> false


  let of_int _ = Finite
  let of_var _ = Finite
  let of_var_string _ = Finite
  let infinity = Infinite
  let exp _ = identity
  let exp_int _ = identity
  let max_of_occurring_constants _ = raise Not_found (* TODO *)
  let log _ = Finite
  let log_of_constant _ = Finite
  let log_of_poly _ = Finite
  let log_of_bound = identity

  let is_infinity = function
    | Finite -> false
    | Infinite -> true


  let is_finite = not % is_infinity

  let to_string ?(pretty = false) ?(termination_only = false) = function
    | Finite -> "Finite"
    | Infinite -> "Infinite"


  let show_finiteness = to_string ~pretty:false ~termination_only:true

  let show ?(pretty = false) ?(complexity = true) ?(termination_only = false) = function
    | Finite -> "YES"
    | Infinite -> "MAYBE"


  let zero = Finite
  let one = Finite

  let max t1 t2 =
    match (t1, t2) with
    | Finite, Finite -> Finite
    | _ -> Infinite


  let maximum bounds = Sequence.reduce ~f:max bounds |? zero
  let add = max
  let mul = max

  let pow t exp =
    if exp = 0 then
      Finite
    else
      t


  let sum b =
    if Sequence.for_all ~f:is_finite b then
      Finite
    else
      Infinite


  let sum_list = sum % Sequence.of_list

  let product b =
    if Sequence.for_all ~f:is_finite b then
      Finite
    else
      Infinite


  let ( + ) = max
  let ( * ) = max
  let ( ** ) = pow
  let substitute var ~replacement = identity
  let substitute_all map = identity
  let substitute_f f = identity
  let indeterminates b = []
  let vars _ = VarSet.empty

  type complexity = t [@@deriving eq]

  let show_complexity = to_string ~pretty:false ~termination_only:true
  let show_complexity_termcomp = show ~pretty:false ~termination_only:false ~complexity:false
  let show_complexity_termcomp_log = show_complexity_termcomp
  let asymptotic_complexity = identity

  let compare_asy b1 b2 =
    match (b1, b2) with
    | Infinite, Infinite -> 0
    | Infinite, _ -> 1
    | _, Infinite -> -1
    | Finite, Finite -> 0


  let min_asy b1 b2 =
    if compare_asy b1 b2 <= 0 then
      b1
    else
      b2


  let ( > ) b1 b2 =
    match (b1, b2) with
    | Infinite, Finite -> Some true
    | _ -> Some false


  let ( < ) = flip ( > )

  let ( >= ) b1 b2 =
    match (b1, b2) with
    | Finite, Infinite -> Some false
    | _ -> Some true


  let ( <= ) = flip ( >= )
  let ( =~= ) = equal

  let is_linear = function
    | Finite -> true
    | Infinite -> false


  let is_polynomial = function
    | Finite -> true
    | Infinite -> false


  let keep_simpler_bound = min_asy
  let eval p valuation = raise (Failure "eval for BinaryBounds not yet implemented")
  let eval_f p valuation = raise (Failure "eval_f for BinaryBounds not yet implemented")
  let degree n = raise (Failure "degree for BinaryBounds not yet implemented")
  let rename map p = raise (Failure "rename for BinaryBounds not yet implemented")
  let coeff_of_var p = raise (Failure "coeff_of_var for BinaryBounds not yet implemented")
  let of_coeff_list p = raise (Failure "of_coeff_list for BinaryBounds not yet implemented")
end
