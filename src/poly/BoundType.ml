open Batteries
open Polynomials
open Util

let logger = Logging.(get Bound)

module Make_BoundOver (Num : PolyTypes.OurNumber)
                      (Poly :
                        sig
                          include PolyTypes.Polynomial with type value = Num.t
                                                        and type valuation = Valuation.Make(Num).t
                                                        and type monomial = Monomials.Make(Num).t
                          val max_of_occurring_constants : t -> Num.t
                        end ) =
  struct
    module Valuation_ = Valuation.Make(Num)
    type valuation = Valuation.Make(Num).t

    type polynomial = Poly.t
    type value = Num.t

    (* Minus Infinity is max of an empty list *)
    (* Infinity is min of an empty list *)
    type t =
      | Infinity
      | Const of Num.t
      | Var of Var.t
      | Neg of t
      | Pow of Num.t * t
      | Sum of t * t
      | Product of t * t
      | Max of t * t
      | Abs of t [@@deriving eq, ord]

    let of_var v = Var v

    let of_constant c = Const c
    let rec get_constant t =
      match t with
      | Infinity -> Num.zero
      | Const c -> c
      | Var var -> Num.zero
      | Neg b -> Num.neg (get_constant b)
      | Pow (n, b) -> Num.pow (get_constant b) (Num.to_int n)
      | Sum (b1, b2) -> Num.add (get_constant b1) (get_constant b2)
      | Product (b1, b2) -> Num.mul (get_constant b1) (get_constant b2)
      | Max (b1, b2) -> Num.max (get_constant b1) (get_constant b2)
      | Abs b -> Num.abs @@ get_constant b

    let get_constant_option t =
      match t with
      | Const x -> Some x
      | _  -> None

    module Constructor =
      struct
        let number = function
          | Infinity -> 0
          | Const _ -> 1
          | Var _ -> 2
          | Neg _ -> 3
          | Pow _ -> 4
          | Sum _ -> 5
          | Product _ -> 6
          | Max _ -> 7
          | Abs _ -> 7

        let (<) b1 b2 =
          number b1 < number b2
      end

    let rec fold ~const ~var ~neg ~plus ~times ~exp ~max ~abs ~inf p =
      let fold_ = fold ~const ~var ~neg ~plus ~times ~exp ~max ~abs ~inf in
      match p with
      | Infinity -> inf
      | Var v -> var v
      | Const c -> const c
      | Max (b1, b2) -> max (fold_ b1) (fold_ b2)
      | Abs b -> abs (fold_ b)
      | Neg b -> neg (fold_ b)
      | Pow (value, n) -> exp value (fold_ n)
      | Sum (b1, b2) -> plus (fold_ b1) (fold_ b2)
      | Product (b1, b2) -> times (fold_ b1) (fold_ b2)
    type complexity =
      | Inf
      | Polynomial of int
      | Exponential of int [@@deriving eq]

    let rec show_complexity = function
      | Inf -> "Infinity"
      | Polynomial 0 -> "O(1)"
      | Polynomial 1 -> "O(n)"
      | Polynomial x -> "O(n^" ^ Int.to_string x ^ ")"
      | Exponential 1 -> "O(EXP)"
      | Exponential x -> "O(EXP^" ^ show_complexity (Exponential (x-1)) ^ ")"

    let show_complexity_termcomp = function
      | Inf -> "MAYBE"
      | Polynomial 0 -> "WORST_CASE(?, O(1))"
      | Polynomial x -> "WORST_CASE(?, O(n^" ^ Int.to_string x ^ "))"
      | Exponential _ -> "WORST_CASE(?, O(EXP))"

    let asymptotic_complexity =
      fold
        ~const:(fun _ -> Polynomial 0)
        ~var:(fun _ -> Polynomial 1)
        ~neg:identity
        ~abs:identity
        ~plus:(fun x y ->
          match (x,y) with
          | (Inf,_) -> Inf
          | (_,Inf) -> Inf
          | (Polynomial x, Polynomial y) -> Polynomial (Int.max x y)
          | (Exponential x, Exponential y) -> Exponential (Int.max x y)
          | (Polynomial x, Exponential y) -> Exponential y
          | (Exponential x, Polynomial y) -> Exponential x
        )
        ~times:(fun x y ->
          match (x,y) with
          | (Inf,_) -> Inf
          | (_,Inf) -> Inf
          | (Polynomial x, Polynomial y) -> Polynomial (Int.add x y)
          | (Exponential x, Exponential y) -> Exponential (Int.max x y)
          | (Polynomial x, Exponential y) -> Exponential y
          | (Exponential x, Polynomial y) -> Exponential x
        )
        ~exp:(fun v b ->
          if (Num.abs v) <= Num.one then
            Polynomial 1
          else
          match b with
            | Inf -> Inf
            | Polynomial x -> Exponential 1
            | Exponential x -> Exponential (Int.succ x)
        )
        ~max:(fun x y ->
          match (x,y) with
          | (Inf,_) -> Inf
          | (_,Inf) -> Inf
          | (Polynomial x, Polynomial y) -> Polynomial (Int.max x y)
          | (Exponential x, Exponential y) -> Exponential (Int.max x y)
          | (Polynomial x, Exponential y) -> Exponential y
          | (Exponential x, Polynomial y) -> Exponential x
        )
        ~inf:Inf

    let rec vars = function
      | Infinity -> VarSet.empty
      | Var v -> VarSet.singleton v
      | Const _ -> VarSet.empty
      | Max (b1, b2) -> VarSet.union (vars b1) (vars b2)
      | Neg b -> vars b
      | Pow (v,b) -> vars b
      | Sum (b1, b2) -> VarSet.union (vars b1) (vars b2)
      | Product (b1, b2) -> VarSet.union (vars b1) (vars b2)
      | Abs b -> vars b

    let is_linear bound =
      let cplx = asymptotic_complexity bound in
        match cplx with
        | (Polynomial n) -> (n == 1)
        | _ -> false

    let is_linear_in_var var bound =
      let maybeOrder =
        fold
          ~const:(const (Some 0))
          ~var:(fun v -> if var = v then (Some 1) else (Some 0))
          ~neg:(identity)
          ~plus:(fun l l' -> Option.Monad.( bind l (fun f -> bind l' (fun s -> return @@ Int.max f s)) ))
          ~times:(fun l l' -> Option.Monad.( bind l (fun f -> bind l' (fun s -> return @@ f + s)) ))
          ~exp:(const @@ const None)
          ~max:(const @@ const None)
          ~abs:(const None)
          ~inf:(Some 0) @@  bound
      in
      match maybeOrder with
        | Some o -> 1 = o
        | None   -> false

    let max_of_occurring_constants bound =
      fold
        ~const:Num.abs
        ~var:(fun _ -> Num.one)
        ~neg:identity
        ~plus:Num.add
        ~times:Num.mul
        ~exp:(fun _ -> raise (Failure "Can not compute max_of_occurring_constants for non-polynomial bounds!"))
        ~max:Num.max
        ~abs:(Num.abs)
        ~inf:(raise (Failure "Can not compute max_of_occurring_constants for non-polynomial bounds!"))
        bound

    let rec show_bound = function
      | Var v -> Var.to_string v
      | Const c -> if Num.Compare.(c < Num.zero) then "("^Num.to_string c^")" else Num.to_string c
      | Infinity -> "inf"
      (*| Max (b1, Max (b2, b3)) -> "max{" ^ show_bound b1 ^ ", " ^ show_bound b2 ^ ", " ^ show_bound b3 ^ "}"*)
      | Max (b1, b2) -> "max([" ^ show_bound b1 ^ ", " ^ show_bound b2 ^ "])"
      | Abs b -> "abs(" ^ show_bound b ^ ")"
      | Neg b ->(
          match b with
          | Const c -> "("^(Num.to_string ( Num.neg c))^")"
          | Neg d -> show_bound d
          | Sum (b1, b2) -> "-(" ^ show_bound (Sum (b1, b2)) ^ ")"
          | Product (b1, b2) -> "-(" ^ show_bound (Product (b1, b2)) ^ ")"
          | Max (b1, b2) -> "min([" ^ show_bound (Neg b1) ^ ", " ^ show_bound (Neg b2) ^ "])"
          | b -> "-(" ^ (show_bound b) ^")"
        )
      | Pow (v, b) -> Num.to_string v ^ "^(" ^ show_bound b ^ ")"
      | Sum (b1, Neg b2) -> show_bound b1 ^ "-" ^ show_bound b2
      | Sum (b1, Const b2) when Num.Compare.(b2 < Num.zero) -> show_bound b1 ^ "-" ^ show_bound (Const (Num.neg b2))
      | Sum (b1, b2) -> show_bound b1 ^ "+" ^ show_bound b2
      | Product (Sum (b1, b2), Sum (b3, b4)) -> "(" ^ show_bound (Sum (b1, b2)) ^ ")*(" ^ show_bound (Sum (b3, b4)) ^ ")"
      | Product (Sum (b1, b2), b3) -> "(" ^ show_bound (Sum (b1, b2)) ^ ")*" ^ show_bound b3
      | Product (b1, Sum (b2, b3)) -> show_bound b1 ^ "*(" ^ show_bound (Sum (b2, b3)) ^ ")"
      | Product (b1, b2) -> show_bound b1 ^ "*" ^ show_bound b2

    let show ?(complexity=true) bound =
      let complexity_str =
        if complexity then " {" ^ (show_complexity % asymptotic_complexity) bound ^ "}" else ""
      in
      show_bound bound ^ complexity_str

    let to_string = show ~complexity:true

    let rec (>) b1 b2 =
      let execute () =
        match (b1, b2) with
        | (_, Infinity) -> Some false
        | (Infinity, _) -> Some true
        | (Neg Infinity, _) -> Some false
        | (_, Neg Infinity) -> Some true
        | (Const c1, Const c2) when Num.Compare.(c1 > c2) -> Some true
        | (b, Const z1) when Num.(equal z1 zero) -> (
          match b with
          | Max (b, _) when b > (Const Num.zero) |? false -> Some true
          | Max (_, b) when b > (Const Num.zero) |? false -> Some true
          | _ -> None
        )
        | (Const z1, b) when Num.(equal z1 zero) -> (
          match b with
          | Neg (Max (b, _)) when b > (Const Num.zero) |? false -> Some true
          | Neg (Max (_, b)) when b > (Const Num.zero) |? false -> Some true
          | _ -> None
        )
        | (b1, b2) -> None
      in
      Logger.with_log logger Logger.DEBUG
                      (fun () -> ">", ["condition", to_string b1 ^ ">" ^ to_string b2])
                      ~result:(Util.option_to_string Bool.to_string)
                      execute
    let rec (>=) b1 b2 =
      let execute () =
        if equal b1 b2 then
          Some true
        else (
          match (b1, b2) with
          | (Infinity, _) -> Some true
          | (_, Neg Infinity) -> Some true
          | (Const c1, Const c2) when Num.Compare.(c1 >= c2) -> Some true
          | (b, Const z1) when Num.(equal z1 zero) -> (
            match b with
            | Max (b, _) when b >= (Const Num.zero) |? false -> Some true
            | Max (_, b) when b >= (Const Num.zero) |? false -> Some true
            | _ -> None
          )
          | (b, Const z1) when Num.(equal z1 zero) -> (
            match b with
            | Neg (Max (b, _)) when b >= (Const Num.zero) |? false -> Some true
            | Neg (Max (_, b)) when b >= (Const Num.zero) |? false -> Some true
            | _ -> None
          )
          | (b1, b2) -> None
        )
      in
      Logger.with_log logger Logger.DEBUG
                      (fun () -> ">=", ["condition", to_string b1 ^ ">=" ^ to_string b2])
                      ~result:(Util.option_to_string Bool.to_string)
                      execute

    let (<) = flip (>)

    let (<=) = flip (>=)

    let (=~=) = equal

    let rec simplify bound =
      let execute () =
        match bound with

        | Infinity -> Infinity

        | Var v -> Var v

        | Const c -> Const c

        (* Simplify terms with negation head *)
        | Neg b -> (
          match simplify b with
          | Const c -> Const (Num.neg c)
          | Sum (b1, b2) -> simplify (Sum (Neg b1, Neg b2))
          | Neg b -> b
          | Product (b1, b2) -> simplify (Product (Neg b1, b2))
          | b -> Neg b
        )

        (* Simplify terms with sum head *)
        | Sum (b1, b2) -> (
          match (simplify b1, simplify b2) with
          | (Const c, b) when Num.(c =~= zero) -> b
          | (b, Const c) when Num.(c =~= zero) -> b
          | (Const c1, Const c2) -> Const Num.(c1 + c2)
          | (Const c1, Sum (Const c2, b)) -> simplify (Sum (Const Num.(c1 + c2), b))
          | (Neg Infinity, Infinity) -> Const Num.zero
          | (Infinity, Neg Infinity) -> Const Num.zero
          | (_, Infinity) -> Infinity
          | (Infinity, _) -> Infinity
          | (_, Neg Infinity) -> Neg Infinity
          | (Neg Infinity, _) -> Neg Infinity
          | (Const c1, Max (Const c2, b)) -> simplify (Max (Const Num.(c1 + c2), Sum (Const c1, b)))
          | (b1, Neg b2) when equal b1 b2 -> Const Num.zero
          | (Neg b1, b2) when equal b1 b2 -> Const Num.zero
          | (b1, b2) when equal b1 b2 -> simplify (Product (Const (Num.of_int 2), b1))
          | (b1, Sum (b2, b3)) when Constructor.(b2 < b1) -> simplify (Sum (b2, Sum (b1, b3)))
          | (Sum (b1, b2), b3) when Constructor.(b3 < b2) -> simplify (Sum (Sum (b1, b3), b2))
          | (b1, b2) when Constructor.(b2 < b1) -> simplify (Sum (b2, b1))
          | (b1, b2) -> Sum (b1, b2)
        )

        (* Simplify terms with product head *)
        | Product (b1, b2) -> (
          match (simplify b1, simplify b2) with
          | (Const c1, Const c2) -> Const (Num.(c1 * c2))
          | (Const c, b) when Num.(c =~= one) -> b
          | (b, Const c) when Num.(c =~= one) -> b
          | (Const c, b) when Num.(c =~= zero) -> Const Num.zero
          | (b, Const c) when Num.(c =~= zero) -> Const Num.zero
          | (Const c, b) when Num.(c =~= neg one) -> simplify (Neg b)
          | (b, Const c) when Num.(c =~= neg one) -> simplify (Neg b)
          | (Infinity, b) when b >= Const Num.zero |? false -> Infinity
          | (b, Infinity) when b >= Const Num.zero |? false -> Infinity
          | (Infinity, b) when b <= Const Num.zero |? false -> Neg Infinity
          | (b, Infinity) when b <= Const Num.zero |? false -> Neg Infinity
          | (Neg Infinity, b) when b >= Const Num.zero |? false -> Neg Infinity
          | (b, Neg Infinity) when b >= Const Num.zero |? false -> Neg Infinity
          | (Neg Infinity, b) when b <= Const Num.zero |? false -> Infinity
          | (b, Neg Infinity) when b <= Const Num.zero |? false -> Infinity
          | (Max (Const zero1, b1), Max (Const zero2, b2)) when Num.(zero1 =~= zero) && Num.(zero2 =~= zero) ->
             simplify (Max (Const Num.zero, Product (b1, b2)))
          | (Max (Const zero1, b1), b2) when Num.(zero1 =~= zero) ->
             simplify (Max (Const Num.zero, Product (b1, b2)))
          | (b1, Product (b2, b3)) when Constructor.(b2 < b1) -> simplify (Product (b2, Product (b1, b3)))
          | (Product (b1, b2), b3) when Constructor.(b3 < b2) -> simplify (Product (Product (b1, b3), b2))
          | (b1, b2) when Constructor.(b2 < b1) -> simplify (Product (b2, b1))
          | (b1, b2) -> Product (b1, b2)
        )

        (* Simplify terms with pow head *)
        | Pow (value, exponent) -> (
           match simplify exponent with
           | exponent when Num.(equal value zero) && (exponent > Const (Num.zero) |? false) -> Const Num.zero
           | _ when Num.(equal value one) -> Const Num.one
           | Infinity when Num.Compare.(value >= Num.of_int 2) -> Infinity
           | Neg Infinity when Num.Compare.(value >= Num.of_int 2) -> Const Num.zero
           | Const c -> Const Num.(pow value (to_int c))
           (* TODO Do not use Num.to_int *)
           | Max (Const c, b) -> simplify (Max (Const (Num.pow value (Num.to_int c)), Pow (value, b)))
           | exponent -> Pow (value, exponent)
        )

        (* Simplify terms with max head *)
        | Max (b1, b2) -> 
          let rec get_max_chain b = match b with
            | Max (b1,b2) -> get_max_chain b1 @ get_max_chain b2
            | b           -> [simplify b]
          in
          let contains_bigger_bound l b = 
            List.exists
              (fun b' -> 
                match b' > b with 
                  | Some true  -> true
                  | Some false -> false
                  | None       -> false)
              l
          in 
          (get_max_chain b1 @ get_max_chain b2)
          |> List.unique ~eq:equal
          |> fun l -> List.fold_left (fun l' b -> if contains_bigger_bound l b then l' else [b]@l') [] l
          |> List.reduce (fun a b -> Max (a,b))
           
        | Abs (Abs b) -> simplify (Abs b)
        | Abs b -> match b >= (Const Num.zero) with
                     | Some true -> b
                     | Some false -> Neg b
                     | None -> Abs b
      in
      Logger.with_log logger Logger.DEBUG
                      (fun () -> "simplify", ["input", to_string bound])
                      ~result:to_string
                      execute

    type outer_t = t
    module BaseMathImpl : (PolyTypes.BaseMath with type t = outer_t) =
      struct
        type t = outer_t

        let zero = Const (Num.zero)

        let one = Const (Num.one)

        let neg bound = simplify (Neg bound)

        let add b1 b2 =
          simplify (Sum (b1, b2))

        let mul b1 b2 =
          simplify (Product (b1, b2))

        let pow bound n =
          bound
          |> Enum.repeat ~times:n
          |> Enum.fold mul one
          |> simplify

      end
    include PolyTypes.MakeMath(BaseMathImpl)

    let sum bounds =
      try
        bounds |> Enum.reduce add |> simplify
      with Not_found -> zero

    let product bounds =
      try
        bounds |> Enum.reduce mul |> simplify
      with Not_found -> one

    let sub t1 t2 =
      simplify (add t1 (neg t2))

    let of_poly =
      Poly.fold ~const:of_constant ~var:of_var ~neg:neg ~plus:add ~times:mul ~pow:pow

    let of_int i = Const (Num.of_int i)

    let to_int poly = raise (Failure "TODO: Not possible")

    let of_var_string str = Var (Var.of_string str)

    let infinity = Infinity

    let minus_infinity = Neg Infinity

    let is_infinity = equal Infinity

    let is_minus_infinity = equal (Neg Infinity)

    let max b1 b2 =
      simplify (Max (b1, b2))

    let min b1 b2 =
      simplify (Neg (Max (neg b1, neg b2)))

    let maximum bounds =
      try
        bounds |> Enum.reduce max |> simplify
      with Not_found -> minus_infinity

    let minimum bounds =
      try
        bounds |> Enum.reduce min |> simplify
      with Not_found -> infinity

    let exp value b =
      if Num.(Compare.(value < zero)) then
        raise (Failure "Zero not allowed in bound pow")
      else
        simplify (Pow (value, b))

    let abs bound =
      simplify (Abs bound)

    let abs_bound get_bound =
      max (abs @@ get_bound `Lower) (abs @@ get_bound `Upper)

    let is_var = function
      | Var _ -> true
      | _ -> false

    let substitute_f substitution bound =
      bound
      |> fold ~const:of_constant ~var:substitution ~neg:neg ~plus:add ~times:mul ~exp:exp ~max:max ~abs:abs ~inf:infinity
      |> simplify

    let set_linear_vars_to_probabilistic_and_rest_to_nonprobabilistic b =
      let all_vars        = vars b in
      let linear_vars     = VarSet.filter (flip is_linear_in_var b) all_vars in

      let setvar v =
        if VarSet.mem v linear_vars then
          Var.set_substitution_kind Var.Probabilistic v
        else
          Var.set_substitution_kind Var.NonProbabilistic v
      in
      substitute_f (of_var % setvar) b

    let substitute var ~replacement =
      substitute_f (fun target_var ->
          if Var.(var =~= target_var) then replacement else of_var target_var
        )

    let substitute_all substitution =
      let module VarMap = Map.Make(Var) in
      substitute_f (fun var ->
          VarMap.find_default (of_var var) var substitution
        )

    type kind = [ `Lower | `Upper ]

    let reverse = function
      | `Lower -> `Upper
      | `Upper -> `Lower

    let selector = function
      | `Lower -> minimum
      | `Upper -> maximum

    let inf = function
      | `Lower -> minus_infinity
      | `Upper -> infinity

    let evaluater lower higher = function
      | `Lower -> lower
      | `Upper -> higher

    let contains_infinity bound =
      fold
        ~const:(fun _ -> false)
        ~var:(fun _ -> false)
        ~neg:identity
        ~plus:(||)
        ~times:(||)
        ~exp:(fun _ -> identity)
        ~max:(||)
        ~abs:(identity)
        ~inf:true
        bound

    let rec appr_substitution kind ~lower ~higher = function
      | Infinity -> Infinity
      | Var v -> evaluater lower higher kind v
      | Const k -> Const k
      | Neg b -> neg (appr_substitution (reverse kind) ~lower ~higher b)
      | Sum (b1, b2) -> appr_substitution kind ~lower ~higher b1 + appr_substitution kind ~lower ~higher b2
      | Product (b1, b2) ->
         List.cartesian_product [`Lower; `Upper] [`Lower; `Upper]
         |> List.map (fun (kind1,kind2) ->
                let multiplication = appr_substitution kind1 ~lower ~higher b1 * appr_substitution kind2 ~lower ~higher b2 in
                if contains_infinity multiplication then
                  inf kind
                else
                  multiplication
              )
         |> List.enum
         |> selector kind
      | Max (b1, b2) -> max (appr_substitution kind ~lower ~higher b1) (appr_substitution kind ~lower ~higher b2)
      | Abs b -> abs (appr_substitution kind ~lower ~higher b)
      | Pow (k,b) -> Pow (k, appr_substitution kind ~lower ~higher b)

    let rec appr_substitution_abs_maybe subf = simplify % function
      | Infinity         -> Infinity
      | Var v            -> (match subf v with
        | Some b -> b
        | None   -> of_var v)
      | Const k          -> Const k
      | Neg b            -> appr_substitution_abs_maybe subf b
      | Sum (b1, b2)     -> appr_substitution_abs_maybe subf b1 + appr_substitution_abs_maybe subf b2
      | Product (b1, b2) -> appr_substitution_abs_maybe subf b1 * appr_substitution_abs_maybe subf b2
      | Max (b1,b2)      -> max (appr_substitution_abs_maybe subf b1)
                                (appr_substitution_abs_maybe subf b2)
      | Abs b            -> abs (appr_substitution_abs_maybe subf b)
      | Pow (k,b)        -> Pow (k, appr_substitution_abs_maybe subf b)

    let replace_if_subst_kind sub_kind b var =
      if Var.get_substitution_kind var = sub_kind then
        Some b
      else
        None

    let appr_substitution_with_sub_kind sub_kind subf =
      appr_substitution_abs_maybe (fun v -> replace_if_subst_kind sub_kind (subf v) v)

    let appr_substition_abs_probabilistic =
      appr_substitution_with_sub_kind Var.Probabilistic

    let appr_substition_abs_nonprobabilistic =
      appr_substitution_with_sub_kind Var.NonProbabilistic

    let appr_substition_abs_all subf =
      appr_substitution_abs_maybe (Option.Monad.return % subf)

    let appr_substitution_probabilistic_and_nonprobabilistic ~probabilistic ~nonprobabilistic =
      appr_substitution_abs_maybe
        (fun v -> if Var.get_substitution_kind v = Var.Probabilistic then Some (probabilistic v) else Some (nonprobabilistic v))

    let appr_substitute_abs v b' =
      appr_substitution_abs_maybe (fun v' -> if v' = v then Some b' else None)

    let degree n = raise (Failure "degree for MinMaxPolynomial not yet implemented")
    let rename map p = raise (Failure "rename for MinMaxPolynomial not yet implemented")
    let eval p valuation = raise (Failure "eval for MinMaxPolynomial not yet implemented")
    let eval_f p valuation = raise (Failure "eval_f for MinMaxPolynomial not yet implemented")
    let of_string p = raise (Failure "of_string for MinMaxPolynomial not yet implemented")
    let coeff_of_var p= raise (Failure "coeff_of_var for MinMaxPolynomial not yet implemented")
    let of_coeff_list p= raise (Failure "of_coeff_list for MinMaxPolynomial not yet implemented")


  end
