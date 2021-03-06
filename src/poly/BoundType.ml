open Batteries
open Polynomials
open Util

let logger = Logging.(get Bound)
let loggerWrapper = Logging.(get BoundWrapper)

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
      | Min of t * t
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
      | Min (b1, b2) -> Num.min (get_constant b1) (get_constant b2)
      | Abs b -> Num.abs @@ get_constant b

    let get_constant_option t =
      match t with
      | Const x -> Some x
      | _  -> None

    let get_var = function
      | Var v -> Some v
      | _     -> None

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
          | Min _ -> 8
          | Abs _ -> 9

        let (<) b1 b2 =
          number b1 < number b2
      end

    let rec fold ~const ~var ~neg ~plus ~times ~exp ~max ~min ~abs ~inf p =
      let fold_ = fold ~const ~var ~neg ~plus ~times ~exp ~max ~min ~abs ~inf in
      match p with
      | Infinity -> inf
      | Var v -> var v
      | Const c -> const c
      | Max (b1, b2) -> max (fold_ b1) (fold_ b2)
      | Min (b1, b2) -> min (fold_ b1) (fold_ b2)
      | Abs b -> abs (fold_ b)
      | Neg b -> neg (fold_ b)
      | Pow (value, n) -> exp value (fold_ n)
      | Sum (b1, b2) -> plus (fold_ b1) (fold_ b2)
      | Product (b1, b2) -> times (fold_ b1) (fold_ b2)
    type complexity =
      | Inf
      | Polynomial of int
      | Exponential of int [@@deriving eq]

    let compare_complexity b1 b2 =
      match (b1,b2) with
      | (Inf, Inf) -> 0
      | (Exponential k1, Exponential k2) -> Int.compare k1 k2
      | (Polynomial k1, Polynomial k2) -> Int.compare k1 k2
      | (Polynomial _, Inf) -> -1
      | (Exponential _, Inf) -> -1
      | (Inf, Polynomial _) -> 1
      | (Inf, Exponential _) -> 1
      | (Polynomial _, Exponential _) -> -1
      | (Exponential _, Polynomial _) -> 1

    let rec show_complexity = function
      | Inf -> "Infinity"
      | Polynomial 0 -> "O(1)"
      | Polynomial 1 -> "O(n)"
      | Polynomial x -> "O(n^" ^ Int.to_string x ^ ")"
      | Exponential 1 -> "O(EXP)"
      | Exponential x -> "O(EXP^" ^ show_complexity (Exponential (x-1)) ^ ")"

    let show_complexity_termcomp ?(tight=false) =
      if tight then
        function
        | Inf -> "MAYBE"
        | Polynomial 0 -> "WORST_CASE(Omega(1), O(1))"
        | Polynomial x -> "WORST_CASE(Omega(n^" ^ Int.to_string x ^ "), O(n^" ^ Int.to_string x ^ "))"
        | Exponential _ -> "WORST_CASE(Omega(EXP), O(EXP))"
      else
        function
        | Inf -> "MAYBE"
        | Polynomial 0 -> "WORST_CASE(?, O(1))"
        | Polynomial x -> "WORST_CASE(?, O(n^" ^ Int.to_string x ^ "))"
        | Exponential _ -> "WORST_CASE(?, O(EXP))"

    let asymptotic_complexity =
      let min_max_helper = (fun x y ->
          match (x,y) with
          | (Inf,_) -> Inf
          | (_,Inf) -> Inf
          | (Polynomial x, Polynomial y) -> Polynomial (Int.max x y)
          | (Exponential x, Exponential y) -> Exponential (Int.max x y)
          | (Polynomial x, Exponential y) -> Exponential y
          | (Exponential x, Polynomial y) -> Exponential x
        )
      in
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
          if Num.Compare.(Num.abs v <= Num.one) then
            Polynomial 1
          else
          match b with
            | Inf -> Inf
            | Polynomial x -> Exponential 1
            | Exponential x -> Exponential (Int.succ x)
        )
        ~max:min_max_helper
        ~min:min_max_helper
        ~inf:Inf

    let rec vars = function
      | Infinity -> VarSet.empty
      | Var v -> VarSet.singleton v
      | Const _ -> VarSet.empty
      | Max (b1, b2) -> VarSet.union (vars b1) (vars b2)
      | Min (b1, b2) -> VarSet.union (vars b1) (vars b2)
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

    let neg_head = function
      | Neg b -> true
      | _     -> false

    let remove_neg_head = function
      | Neg b -> b
      | b     -> b

    let is_one = function
      | Const k -> Num.equal k Num.one
      | _       -> false

    let is_linear_in_var var bound =
      let maybeOrder =
        fold
          ~const:(const (Some 0))
          ~var:(fun v -> if var = v then (Some 1) else (Some 0))
          ~neg:(identity)
          ~plus:(fun l l' -> Option.Monad.( bind l (fun f -> bind l' (fun s -> return @@ Int.max f s)) ))
          ~times:(fun l l' -> Option.Monad.( bind l (fun f -> bind l' (fun s -> return @@ f + s)) ))
          ~exp:(const @@ const None)
          ~max:(fun l l' -> if l = Some 0 && l' = Some 0 then Some 0 else None)
          ~min:(fun l l' -> if l = Some 0 && l' = Some 0 then Some 0 else None)
          ~abs:(fun l -> if l = Some 0 then Some 0 else None)
          ~inf:(Some 0) @@  bound
      in
      match maybeOrder with
        | Some o -> 1 = o || 0 = o
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
        ~min:Num.min
        ~abs:(Num.abs)
        ~inf:(raise (Failure "Can not compute max_of_occurring_constants for non-polynomial bounds!"))
        bound

    let rec show_bound = function
      | Var v -> Var.to_string v
      | Const c -> if Num.Compare.(c < Num.zero) then "("^Num.to_string c^")" else Num.to_string c
      | Infinity -> "inf"
      (*| Max (b1, Max (b2, b3)) -> "max{" ^ show_bound b1 ^ ", " ^ show_bound b2 ^ ", " ^ show_bound b3 ^ "}"*)
      | Max (b1, b2) -> "max([" ^ show_bound b1 ^ ", " ^ show_bound b2 ^ "])"
      | Min (b1, b2) -> "min([" ^ show_bound b1 ^ ", " ^ show_bound b2 ^ "])"
      | Abs b -> "abs(" ^ show_bound b ^ ")"
      | Neg b ->(
          match b with
          | Const c -> "("^(Num.to_string ( Num.neg c))^")"
          | Neg d -> show_bound d
          | Sum (b1, b2) -> "-(" ^ show_bound (Sum (b1, b2)) ^ ")"
          | Product (b1, b2) -> "-(" ^ show_bound (Product (b1, b2)) ^ ")"
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

    let to_constructor_string =
      fold
        ~const:(fun c -> "Const (" ^ Num.to_string c ^ ")")
        ~var:(fun v -> "Var (" ^ Var.to_string v ^ ")")
        ~neg:(fun s -> "Neg (" ^ s ^ ")")
        ~abs:(fun s -> "abs (" ^ s ^ ")")
        ~plus:(fun s1 s2 -> "Plus (" ^ s1 ^ ", " ^ s2 ^ ")")
        ~times:(fun s1 s2 -> "times (" ^ s1 ^ ", " ^ s2 ^ ")")
        ~max:(fun s1 s2 -> "max (" ^ s1 ^ ", " ^ s2 ^ ")")
        ~min:(fun s1 s2 -> "min (" ^ s1 ^ ", " ^ s2 ^ ")")
        ~exp:(fun b s -> "Exp (" ^ Num.to_string b ^ ", " ^ s ^ ")")
        ~inf:("Inf")

    let opt_bool_to_bool = function
      | Some true -> true
      | _         -> false

    type boundtype = t
    module CompCacheTable =
      Hashtbl.Make
        (struct
          type t = bool * boundtype * boundtype
          let hash (b,t,t') = Hashtbl.hash (b,to_string t, to_string t')
          let equal (b1,t1,t1') (b2,t2,t2') = Bool.equal b1 b2 && equal t1 t2 && equal t1' t2'
        end)

    let gt_cache: (bool option) CompCacheTable.t = CompCacheTable.create 1000
    let gte_cache: (bool option) CompCacheTable.t = CompCacheTable.create 1000

    let rec greater ~(opt_invariants:([`GE | `GT] -> t -> t -> bool option) option) ~assume_vars_nonnegative b1 b2 =
      let execute () =
        let cache_entry = CompCacheTable.find_option gt_cache (assume_vars_nonnegative, b1, b2) in
        if Option.is_none opt_invariants && Option.is_some cache_entry then
             Option.get cache_entry
        else
          let helper b1 b2 =
            match (b1, b2) with
            | (Var v, Const c) when Num.Compare.(c < Num.zero) && assume_vars_nonnegative ->  Some true
            | (Infinity, _) -> Some true
            | (_, Neg Infinity) -> Some true
            | (Const c1, Const c2) when Num.Compare.(c1 > c2) -> Some true
            | (Abs b, Const c) when (Num.Compare.(c < Num.zero) || opt_bool_to_bool (greater ~opt_invariants:opt_invariants ~assume_vars_nonnegative:assume_vars_nonnegative b (Const c))
                                                                || opt_bool_to_bool (greater ~opt_invariants:opt_invariants ~assume_vars_nonnegative:assume_vars_nonnegative (Neg (Const c)) b)) -> Some true

            | (Sum (b1,b2), b3) when
                (greater ~opt_invariants:opt_invariants ~assume_vars_nonnegative:assume_vars_nonnegative b1 b3 = Some true
                  && greater_or_equal ~opt_invariants:opt_invariants ~assume_vars_nonnegative:assume_vars_nonnegative b2 b3 = Some true
                  && greater_or_equal ~opt_invariants:opt_invariants ~assume_vars_nonnegative:assume_vars_nonnegative b1 (Const Num.zero) = Some true
                  && greater_or_equal ~opt_invariants:opt_invariants ~assume_vars_nonnegative:assume_vars_nonnegative b2 (Const Num.zero) = Some true) -> Some true
            | (Sum (b1,b2), b3) when
                (greater_or_equal ~opt_invariants:opt_invariants ~assume_vars_nonnegative:assume_vars_nonnegative b1 b3 = Some true
                  && greater ~opt_invariants:opt_invariants ~assume_vars_nonnegative:assume_vars_nonnegative b2 b3 = Some true
                  && greater_or_equal ~opt_invariants:opt_invariants ~assume_vars_nonnegative:assume_vars_nonnegative b1 (Const Num.zero) = Some true
                  && greater_or_equal ~opt_invariants:opt_invariants ~assume_vars_nonnegative:assume_vars_nonnegative b2 (Const Num.zero) = Some true) -> Some true

            | (Sum (b1,b2),b3) when greater ~opt_invariants:opt_invariants ~assume_vars_nonnegative:assume_vars_nonnegative b2 (Const Num.zero) = Some true && equal b1 b3-> Some true
            | (Sum (b1,b2),b3) when greater ~opt_invariants:opt_invariants ~assume_vars_nonnegative:assume_vars_nonnegative b1 (Const Num.zero) = Some true && equal b2 b3-> Some true

            | (Max (b1,b2), b3) when (greater ~opt_invariants:opt_invariants ~assume_vars_nonnegative:assume_vars_nonnegative b1 b3 = Some true || greater ~opt_invariants:opt_invariants ~assume_vars_nonnegative:assume_vars_nonnegative b2 b3 = Some true) -> Some true
            | (Max (b1,b2), b3) when (greater ~opt_invariants:opt_invariants ~assume_vars_nonnegative:assume_vars_nonnegative b3 b1 = Some true && greater ~opt_invariants:opt_invariants ~assume_vars_nonnegative:assume_vars_nonnegative b3 b2 = Some true) -> Some false

            | (Min (b1,b2), b3) when (greater ~opt_invariants:opt_invariants ~assume_vars_nonnegative:assume_vars_nonnegative b1 b3 = Some true && greater ~opt_invariants:opt_invariants ~assume_vars_nonnegative:assume_vars_nonnegative b2 b3 = Some true) -> Some true
            | (Min (b1,b2), b3) when (greater ~opt_invariants:opt_invariants ~assume_vars_nonnegative:assume_vars_nonnegative b3 b1 = Some true || greater ~opt_invariants:opt_invariants ~assume_vars_nonnegative:assume_vars_nonnegative b3 b2 = Some true) -> Some false

            | (b, Const z1) when Num.(equal z1 zero) -> (
              match b with
              | Max (b, _) when greater ~opt_invariants:opt_invariants ~assume_vars_nonnegative:assume_vars_nonnegative b (Const Num.zero) |? false -> Some true
              | Max (_, b) when greater ~opt_invariants:opt_invariants ~assume_vars_nonnegative:assume_vars_nonnegative b (Const Num.zero) |? false -> Some true
              | _ -> None
            )
            | (Const z1, b) when Num.(equal z1 zero) -> (
              match b with
              | Neg (Max (b, _)) when greater ~opt_invariants:opt_invariants ~assume_vars_nonnegative:assume_vars_nonnegative b (Const Num.zero) |? false -> Some true
              | Neg (Max (_, b)) when greater ~opt_invariants:opt_invariants ~assume_vars_nonnegative:assume_vars_nonnegative b (Const Num.zero) |? false -> Some true
              | _ -> None
            )
            | (b1, b2) -> None
          in
          let inv = Option.Monad.bind opt_invariants (fun f -> f `GT b1 b2) in
          (
            match inv with
              | Some b -> Some b
              | None ->
                match equal b1 b2 with
                | true  -> Some false
                | false -> match helper b1 b2 with
                    | Some b -> Some b
                    | None   -> helper b2 b1 |> Option.map not
          )
          |> tap (fun r ->  if Option.is_none opt_invariants then CompCacheTable.add gt_cache (assume_vars_nonnegative, b1, b2) r)
      in
      Logger.with_log logger Logger.DEBUG
                      (fun () -> ">", ["condition", to_string b1 ^ ">" ^ to_string b2])
                      ~result:(Util.option_to_string Bool.to_string)
                      execute

    and greater_or_equal ~(opt_invariants: ([`GE | `GT] -> t -> t -> bool option) option) ~assume_vars_nonnegative b1 b2 =
      let rec_call = greater_or_equal ~opt_invariants:opt_invariants ~assume_vars_nonnegative:assume_vars_nonnegative in
      let execute () =
        let cache_entry = CompCacheTable.find_option gte_cache (assume_vars_nonnegative, b1, b2) in
        if Option.is_none opt_invariants && Option.is_some cache_entry then
             Option.get cache_entry
        else
          let helper b1 b2 =
            if equal b1 b2 then
              Some true
            else (
              match (b1, b2) with
              | (Var v, Const c) when Num.Compare.(c <= Num.zero) && assume_vars_nonnegative -> Some true
              | (Const c, Var v) when Num.Compare.(c < Num.zero) && assume_vars_nonnegative -> Some false

              | (Abs _, Const c) when Num.equal c Num.zero -> Some true
              | (Abs b1, b2) when equal b1 b2 -> Some true

              | (Const c, Abs _) when Num.equal c Num.zero -> Some false
              | (Infinity, _) -> Some true
              | (Sum (Abs _, b), b2) when equal b b2 -> Some true
              | (Sum (b1,b2), b3) when
                  (rec_call b1 b3 = Some true && rec_call b2 b3 = Some true
                    && rec_call b1 (Const Num.zero) = Some true && rec_call b2 (Const Num.zero) = Some true) -> Some true
              | (Sum (b1,b2), b3) when
                  (rec_call b1 b3 = Some true && rec_call b2 (Const Num.zero) = Some true)-> Some true
              | (Sum (b2,b1), b3) when
                  (rec_call b1 b3 = Some true && rec_call b2 (Const Num.zero) = Some true)-> Some true

              | (_, Neg Infinity) -> Some true

              | (Max (b1,b2), b3) when (rec_call b1 b3 = Some true
                  || rec_call b2 b3 = Some true) -> Some true

              | (Min (b1,b2), b3) when (rec_call b1 b3 = Some true
                  && rec_call b2 b3 = Some true) -> Some true

              | ((Product (Const c1,b1)),Product (Const c2, b2)) when Num.Compare.(c1 >= c2) && (equal b1 b2) -> Some true
              | ((Product (Const c1,b1)),Product (b2, Const c2)) when Num.Compare.(c1 >= c2) && (equal b1 b2) -> Some true
              | ((Product (b1,Const c1)),Product (Const c2, b2)) when Num.Compare.(c1 >= c2) && (equal b1 b2) -> Some true
              | ((Product (b1,Const c1)),Product (b2, Const c2)) when Num.Compare.(c1 >= c2) && (equal b1 b2) -> Some true

              | (Product (Const c1, b2), b) when Num.Compare.(c1 >= Num.one) && (rec_call b2 b = Some true) -> Some true
              | (b,Product (Const c1, b2)) when Num.Compare.(Num.one >= c1 && c1 >= Num.zero) && (rec_call b b2 = Some true) -> Some true

              (* Check Positivity *)
              | (Product (b1,b2), Const c) when
                  Num.Compare.(c <= Num.zero) && rec_call b1 (Const Num.zero) = Some true
                    && rec_call b2 (Const Num.zero) = Some true -> Some true
              | (Const c, Product (b1,b2)) when
                  Num.Compare.(c < Num.zero) && rec_call b1 (Const Num.zero) = Some true
                    && rec_call b2 (Const Num.zero) = Some true -> Some false

              | (Const c1, Const c2) when Num.Compare.(c1 >= c2) -> Some true
              | (b, Const z1) when Num.(equal z1 zero) -> (
                match b with
                | Max (b, _) when rec_call b (Const Num.zero) |? false -> Some true
                | Max (_, b) when rec_call b (Const Num.zero) |? false -> Some true
                | _ -> None
              )
              | (b, Const z1) when Num.(equal z1 zero) -> (
                match b with
                | Neg (Max (b, _)) when rec_call b (Const Num.zero) |? false -> Some true
                | Neg (Max (_, b)) when rec_call  b (Const Num.zero) |? false -> Some true
                | _ -> None
              )
              | (b1, b2) -> None
            )
        in
        let gt b1 b2 = greater ~opt_invariants:opt_invariants ~assume_vars_nonnegative:assume_vars_nonnegative b1 b2 in
        let lt b1 b2 = greater ~opt_invariants:opt_invariants ~assume_vars_nonnegative:assume_vars_nonnegative b2 b1 in
        let inv = Option.Monad.bind opt_invariants (fun f -> f `GE b1 b2) in
        (
          match inv with
            | Some b -> Some b
            | None   ->
                if gt b1 b2 = Some true then
                  Some true
                else
                  (
                    if lt b1 b2 = Some true then
                      Some false
                    else
                      (
                        match helper b1 b2 with
                        | Some true  -> Some true
                        | Some false -> Some false
                        | _          -> None
                      )
                  )
        )
        |> tap (fun r -> if Option.is_none opt_invariants then CompCacheTable.add gte_cache (assume_vars_nonnegative, b1, b2) r)
      in
      Logger.with_log logger Logger.DEBUG
                      (fun () -> ">=", ["condition", to_string b1 ^ ">=" ^ to_string b2])
                      ~result:(Util.option_to_string Bool.to_string)
                      execute

    let simple_log str = Logger.log logger Logger.DEBUG (fun () -> str, [])

    let is_infinity = equal Infinity

    let is_minus_infinity = equal (Neg Infinity)

    module SimplifyCacheTable =
      Hashtbl.Make
        (struct
          type t = bool * boundtype
          let hash (b,t) = Hashtbl.hash (b,to_string t)
          let equal (b1,t1) (b2,t2) = Bool.equal b1 b2 && equal t1 t2
        end)
    let simplify_cache: t SimplifyCacheTable.t = SimplifyCacheTable.create 1000

    (*  Simplify a given bound. Use assume-vars_nonnegative = true to simplify bounds with absolute values. opt_invariants may contain a function that may provide additional,
        e.g. by using an SMT solving backend *)
    let rec simplify_ ~opt_invariants ~assume_vars_nonnegative:assume_vars_nonnegative bound =
      let simplify_rec_call = simplify_ ~opt_invariants:opt_invariants ~assume_vars_nonnegative:assume_vars_nonnegative in
      (* Get chains of max or min expressions. E.g. Max(Max(b1,b2), b3) should be represented as [b1, b2, b3] *)
      let rec get_minmax_chain t' b = match (t',b) with
        | (`Max,Max (b1,b2))        -> get_minmax_chain t' b1       @ get_minmax_chain t' b2
        | (`Min,Min (b1,b2))        -> get_minmax_chain t' b1       @ get_minmax_chain t' b2
        | (`Min, Neg (Max (b1,b2))) -> get_minmax_chain t' (Neg b1) @ get_minmax_chain t' (Neg b2)
        | (`Max, Neg (Min (b1,b2))) -> get_minmax_chain t' (Neg b1) @ get_minmax_chain t' (Neg b2)
        | (`Min, Neg (Min (b1,b2))) -> [simplify_rec_call @@ Max(Neg b1, Neg b2)]
        | (`Max, Neg (Max (b1,b2))) -> [simplify_rec_call @@ Max(Neg b1, Neg b2)]
        | (_,b)                     -> [simplify_rec_call b]

      (* Similar to get_minmax_chain but for sums and products*)
      and get_op_chain t b =
        match (t,b) with
        | (`Sum, Sum (b1,b2))             -> get_op_chain t b1 @ get_op_chain t b2
        | (`Product, Product (b1,b2))     -> get_op_chain t b1 @ get_op_chain t b2
        | (`Product, Neg b)               -> Const Num.minus_one :: [simplify_rec_call b]
        | (_, b)                          -> [simplify_rec_call b]

      and get_minmax_chain_tuple t' (b1,b2) =
        get_minmax_chain t' b1 @ get_minmax_chain t' b2

      (* Reverse operation to get_minmax_chain, e.g. construct a chain *)
      and construct_minmax_chain t' bs =
        let sorted = List.sort (fun b1 b2 -> String.compare (show_bound b1) (show_bound b2)) bs in
        let default_min_max = function
          | `Max -> Neg Infinity
          | `Min -> Infinity
        in
        try
          List.reduce
            (match t' with
              | `Min -> fun b1 b2 -> Min (b1,b2)
              | `Max -> fun b1 b2 -> Max (b1,b2)) sorted
        with Invalid_argument _ -> default_min_max t'

      (* Reverse to get_op_chain, e.g. construct a chain *)
      and construct_op_chain t bs =
        (* Sort terms to allow for better equality checking of similar terms. String comparison is kind of arbitrary *)
        let sorted = List.sort (fun b1 b2 -> String.compare (show_bound b1) (show_bound b2)) bs in
        match t with
          | `Sum ->
              (try List.reduce (fun b1 b2 -> Sum (b1,b2)) sorted
              with Invalid_argument _  -> of_constant (Num.zero))
          | `Product ->
              (try List.reduce (fun b1 b2 -> Product (b1,b2)) sorted
              with Invalid_argument _  -> of_constant (Num.one))
      in

      (* This function simplifies bounds with both min and max heads.
        This is convenient since in both cases the same reasoning can be applied with marginal differences.
      *)
      let min_max_helper t (b1,b2) =
          let keep_least_greatest_bounds t bs =
            (* for a chain [b1, b2,..] find a subset [b'_1, b'_2, ...] such that
                max[b1, b2,..] = max [b'_1, ..] (in the case of t=`Max)
                , or min[b1, b2,..] = min[b'_1,..]
            *)
            let comperator = match t with
              | `Max  -> greater_or_equal ~opt_invariants:opt_invariants ~assume_vars_nonnegative:assume_vars_nonnegative
              | `Min  -> flip (greater_or_equal ~opt_invariants:opt_invariants ~assume_vars_nonnegative:assume_vars_nonnegative)
            in
            let rec helper selected : boundtype list list =
              let is_bounded = List.map (fun b -> b, List.exists (fun b' -> comperator b' b = Some true) selected) bs in
              if List.for_all Tuple2.second is_bounded then
                (* if all bounds are bounded by the selected subset return the subset into the monad *)
                ListMonad.Monad.pure selected
              else
                (* Otherwise, nondeterministically choose a bound that is not-bounded by the selected bounds,
                  and add it to the selected
                *)
                ListMonad.Monad.(
                  List.filter (not % Tuple2.second) is_bounded
                  >>= (helper % flip List.cons selected % Tuple2.first)
                )
            in
            helper []
            |> List.sort (fun a b -> Int.compare (List.length a) (List.length b))
            |> List.hd
          in
          let inverse_type = function
            | `Min -> `Max
            | `Max -> `Min
          in
          let is_type t' b =
            match (t',b) with
              | (`Max, Max _) -> true
              | (`Min, Min _) -> true
              | _             -> false
          in
          let extract_bounds = function
            | Min (b1,b2) -> (b1,b2)
            | Max (b1,b2) -> (b1,b2)
            | _           -> raise (Failure "Bound head should either be Min or Max")
          in
          (* Simplify alternating min max bounds, e.g. max[min[., c], c] = c *)
          let simplify_alt_minmax bs =
            let (inverse_types, not_inverse_types) = List.partition (is_type (inverse_type t)) bs in

            let removed_doubles_in_inverse_types =
              inverse_types
              |> List.map (get_minmax_chain_tuple (inverse_type t) % extract_bounds)
              |> List.filter (not % List.exists (fun b -> List.exists (equal b) bs))
              |> List.map (construct_minmax_chain (inverse_type t))
            in

            List.append not_inverse_types removed_doubles_in_inverse_types
            |> List.unique ~eq:equal
          in

          (get_minmax_chain t b1 @ get_minmax_chain t b2)
          |> List.unique ~eq:equal
          |> keep_least_greatest_bounds t
          |> simplify_alt_minmax
          |> construct_minmax_chain t
      in
      let execute () =
        let cache_entry = SimplifyCacheTable.find_option simplify_cache (assume_vars_nonnegative, bound) in
        (* Use cache only when not using opt_invariants since this hint giving function is a black box *)
        if Option.is_none opt_invariants && Option.is_some cache_entry then
           Option.get cache_entry
        else
          match bound with

          | Infinity -> Infinity

          | Var v    -> Var v

          | Const c  -> Const c

          (* Simplify terms with negation head *)
          | Neg b -> (
            match simplify_rec_call b with
            | Const c -> Const (Num.neg c)
            | Sum (b1, b2) -> simplify_rec_call (Sum (Neg b1, Neg b2))
            | Neg b -> b
            | Product (b1, b2) -> simplify_rec_call (Product (Neg b1, b2))
            | b -> Neg b
          )

          (* Simplify terms with sum head *)
          | Sum (b1, b2) -> (
            (* This function is taken from the original simplify method.
                Todo: Seamlessly integrate with the 'new' version
            *)
            let rec simplify_bi = function
              | Sum (b1,b2) ->
                (match (b1, b2) with
                | (Const c1, Max (Const c2, b)) -> simplify_bi (Max (Const Num.(c1 + c2), Sum (Const c1, b)))
                | (Max (Const c2, b), Const c1) -> simplify_bi (Max (Const Num.(c1 + c2), Sum (Const c1, b)))
                | (Const c1, Min (Const c2, b)) -> simplify_bi (Min (Const Num.(c1 + c2), Sum (Const c1, b)))
                | (Min (Const c2, b), Const c1) -> simplify_bi (Min (Const Num.(c1 + c2), Sum (Const c1, b)))
                | (b1, Sum (b2, b3)) when Constructor.(b2 < b1) -> simplify_bi (Sum (b2, Sum (b1, b3)))
                | (Sum (b1, b2), b3) when Constructor.(b3 < b2) -> simplify_bi (Sum (Sum (b1, b3), b2))
                | (b1, b2) -> Sum (b1, b2))
              | b -> b
            in
            let sum_chain =
              get_op_chain `Sum b1 @ get_op_chain `Sum b2
              |> List.filter (not % equal (Const Num.zero))
            in
            match (List.exists is_infinity sum_chain, List.exists is_minus_infinity sum_chain) with
            | (true, false) -> Infinity
            | (false, true) -> Neg Infinity
            | _             ->
              (* Merge addends that are a product of the same bound with different coefficients *)
              let combine_chain_elements_with_coeffs =
                let get_coeff_elem = function
                  | Product (Const c, b) -> (b,c)
                  | Product (b, Const c) -> (b,c)
                  | Const k              -> (Const Num.one, k)
                  | Neg b                -> (b, Num.neg Num.one)
                  | b                    -> (b,Num.one)
                in
                sum_chain
                |> List.map get_coeff_elem
                |> List.fold_left
                    (fun list (b,c) ->
                      try
                        let i = fst @@ List.findi (fun i -> equal b % fst) list in
                        List.modify_at i (fun (b,c') -> (b,Num.(c + c'))) list
                      with Not_found -> List.cons (b,c) list)
                    []
                |> List.map (fun (b,c) -> Product (Const c, b) |> simplify_rec_call)
              in
              (* Finally take the chain with possibly merged addends and construct a bound before applying the 'old' approach to it *)
              combine_chain_elements_with_coeffs
              |> construct_op_chain `Sum
              |> function
                  | Sum (b1,b2) -> simplify_bi @@ Sum (b1,b2)
                  | b -> b
            )

          (* Simplify terms with product head *)
          | Product (b1, b2) ->  (
            (* In the case of infinity remove all factors of the product chain while maintaining the sign *)
            let chain_eliminate_redundant_infinity =
              let chain = get_op_chain `Product b1 @ get_op_chain `Product b2 in
              if List.exists (fun b -> is_infinity b || is_minus_infinity b) chain then
                chain
                |> List.enum
                |> Enum.filter (not % is_infinity)
                |> Enum.map
                    (fun b ->
                      match (greater ~opt_invariants:opt_invariants ~assume_vars_nonnegative:assume_vars_nonnegative (Const Num.zero) b,
                             greater ~opt_invariants:opt_invariants ~assume_vars_nonnegative:assume_vars_nonnegative b (Const Num.zero))
                      with
                      | (Some true, _) -> Const (Num.(neg one))
                      | (_, Some true) -> Const (Num.one)
                      | _              -> b
                    )
                |> List.of_enum
                |> fun l -> List.cons Infinity l
              else
                chain
            in

            (* Partition the chain in sets of constants and non-constant factors *)
            let get_const = function
              | Const c       -> Some c
              | Neg (Const c) -> Some (Num.neg c)
              | _             -> None
            in
            let all_non_consts = List.filter (Option.is_none % get_const) chain_eliminate_redundant_infinity in
            let all_consts = List.map Option.get @@ List.filter Option.is_some @@ List.map get_const chain_eliminate_redundant_infinity in

            (* Get the coefficient of the complete chain by multiplying all of its constant values *)
            let const =
              let c = List.fold_left Num.mul Num.one all_consts in
              if List.exists (fun b -> is_infinity b || is_minus_infinity b) all_non_consts then
                (* eliminate constants if product contains infinity *)
                if Num.Compare.(c = Num.zero) then
                  c
                else if Num.Compare.(c > Num.zero) then
                  Num.one
                else
                  Num.(neg one)
              else
                c
            in

            let non_const_chain = construct_op_chain `Product all_non_consts in

            if Num.(equal const zero) then
              Const const
            else if Num.(equal const one) then
              non_const_chain
            else if Num.(equal const (neg one)) then
              (Neg non_const_chain)
            else if List.is_empty all_non_consts then
              Const const
            else
              Product (Const const, non_const_chain)
          )

          (* Simplify terms with pow head *)
          | Pow (value, exponent) -> (
            match simplify_rec_call exponent with
            | exponent when Num.(equal value zero) && (greater ~opt_invariants:opt_invariants ~assume_vars_nonnegative:assume_vars_nonnegative exponent (Const Num.zero) |? false) -> Const Num.zero
            | _ when Num.(equal value one) -> Const Num.one
            | Infinity when Num.Compare.(value >= Num.of_int 2) -> Infinity
            | Neg Infinity when Num.Compare.(value >= Num.of_int 2) -> Const Num.zero
            | Const c -> Const Num.(pow value (to_int c))
            (* TODO Do not use Num.to_int *)
            | Max (Const c, b) -> simplify_rec_call (Max (Const (Num.pow value (Num.to_int c)), Pow (value, b)))
            | exponent -> Pow (value, exponent)
          )

          (* Simplify terms with max head *)
          | Max (b1, b2) -> min_max_helper `Max (b1,b2)

          | Min (b1,b2) -> min_max_helper `Min (b1,b2)

          (* Simplify terms with abs head *)
          | Abs (Neg b) -> simplify_rec_call (Abs b)
          | Abs (Abs b) -> simplify_rec_call (Abs b)
          | Abs (Product (b1,b2)) ->
              (* Partition the product chain into factors X_i which are greater or equal 0, i.e. |X_i| = X_i,
                 factors Y_i which are less or equal than 0, i.e. |Y_i| = -Y_i
                 , and factors Z_i for which none of this is provable.
                 Then the the original bound can be represented as X_1 * X_2 * (-Y_1) * (Y_2) * .. * abs(Z_1) * abs(Z_2) * ... *)
              let chain   = get_op_chain `Product b1 @ get_op_chain `Product b2 in
              let (all_geq0, all_leq0, all_others) =
                List.fold_left
                  (fun (geq0, leq0, others) e ->
                      if greater_or_equal ~opt_invariants:opt_invariants ~assume_vars_nonnegative:assume_vars_nonnegative e (Const Num.zero) =  Some true then
                        (e::geq0, leq0, others)
                      else if greater_or_equal ~opt_invariants:opt_invariants ~assume_vars_nonnegative:assume_vars_nonnegative (Const Num.zero) e =  Some true then
                        (geq0, e::leq0, others)
                      else
                        (geq0, leq0, e::others)
                  ) ([], [], []) chain
              in

              simplify_rec_call @@
                (* Construct Chains *)
                Product(Product(construct_op_chain `Product all_geq0, construct_op_chain `Product @@ List.map (fun b -> Neg b) all_leq0),
                        construct_op_chain `Product @@ List.map (fun b -> Abs b) all_others)

          (* Check if a bound is provably >=0 or <=0. If this is the case elimnate Abs accordingly *)
          | Abs b -> match greater_or_equal ~opt_invariants:opt_invariants ~assume_vars_nonnegative:assume_vars_nonnegative b (Const Num.zero) with
                      | Some true -> simplify_rec_call b
                      | Some false -> simplify_rec_call (Neg b)
                      | None -> Abs (simplify_rec_call b)
        (* Add to cache *)
        |> tap (fun t -> if Option.is_none opt_invariants then SimplifyCacheTable.add simplify_cache (assume_vars_nonnegative, bound) t)
      in
      Logger.with_log logger Logger.DEBUG
                      (fun () -> "simplify_", ["input", to_string bound])
                      ~result:to_string
                      execute

    (* Wrapper for simplify to improve logging *)
    let simplifywrapper callerstring ~opt_invariants ~assume_vars_nonnegative b =
      Logger.log loggerWrapper Logger.DEBUG (fun () -> callerstring , ["b",to_string b]);
      simplify_ ~opt_invariants:opt_invariants ~assume_vars_nonnegative:assume_vars_nonnegative b

    let simplify b =
      simplifywrapper "simplifywrapper" ~opt_invariants:None ~assume_vars_nonnegative:false b

    (* Wrapper for simplify to improve logging but assumes that all variables are elements of a nonnegative domain *)
    let simplify_vars_nonnegative b =
      simplifywrapper "simplifyabswrapper" ~opt_invariants:None ~assume_vars_nonnegative:true b

    (* Wrapper for simplify to improve logging but uses the provided hints *)
    let simplify_opt_invariants opt_invariants b =
      simplifywrapper "simplifyoptinvariantswrapper" ~opt_invariants:(Some opt_invariants) ~assume_vars_nonnegative:true b


    let (>)  = greater          ~opt_invariants:None ~assume_vars_nonnegative:false
    let (>=) = greater_or_equal ~opt_invariants:None ~assume_vars_nonnegative:false

    let (<)  = flip (>)
    let (<=) = flip (>=)

    let (=~=) = equal

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

    let of_intpoly =
      Polynomial.fold ~const:(of_constant % Num.of_ourint) ~var:of_var ~neg:neg ~plus:add ~times:mul ~pow:pow

    let of_int i = Const (Num.of_int i)

    let to_int poly = raise (Failure "TODO: Not possible")

    let of_var_string str = Var (Var.of_string str)

    let infinity = Infinity

    let minus_infinity = Neg Infinity

    let max b1 b2 =
      simplify (Max (b1, b2))

    let min b1 b2 =
      simplify (Min (b1, b2))

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
      let worst_case_estimation = max (abs @@ get_bound `Lower) (abs @@ get_bound `Upper) in

      (match (get_bound `Lower >= (Const Num.zero), get_bound `Upper >= (Const Num.zero)) with
      | (Some true, _) -> abs @@ get_bound `Upper
      | _ ->
          match (get_bound `Lower <= zero, get_bound `Upper <= zero) with
          | (_, Some true) -> abs @@ get_bound `Lower
          | _                      -> worst_case_estimation)
      |> simplify_vars_nonnegative

    let is_var = function
      | Var _ -> true
      | _ -> false

    let substitute_f substitution bound =
      bound
      |> fold ~const:of_constant ~var:substitution ~neg:neg ~plus:add ~times:mul ~exp:exp ~max:max ~min:min ~abs:abs ~inf:infinity
      |> simplify

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
        ~min:(||)
        ~abs:(identity)
        ~inf:true
        bound

    let rec appr_substitution kind ~lower ~higher = function
      | Infinity         -> Infinity
      | Var v            -> evaluater lower higher kind v
      | Const k          -> Const k
      | Neg b            -> neg (appr_substitution (reverse kind) ~lower ~higher b)
      | Sum (b1, b2)     -> appr_substitution kind ~lower ~higher b1 + appr_substitution kind ~lower ~higher b2
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
      | Min (b1, b2) -> min (appr_substitution kind ~lower ~higher b1) (appr_substitution kind ~lower ~higher b2)
      | Abs b -> abs (appr_substitution kind ~lower ~higher b)
      | Pow (k,b) -> Pow (k, appr_substitution kind ~lower ~higher b)

    let rec appr_substitution_abs_maybe subf b =
      (match b with
      | Infinity         -> Infinity
      | Var v            -> (match subf v with
        | Some b -> b
        | None   -> of_var v)
      | Const k          -> Const (Num.abs k)
      | Neg b            -> appr_substitution_abs_maybe subf b
      | Sum (b1, b2)     -> appr_substitution_abs_maybe subf b1 + appr_substitution_abs_maybe subf b2
      | Product (b1, b2) -> appr_substitution_abs_maybe subf b1 * appr_substitution_abs_maybe subf b2
      | Max (b1,b2)      -> max (appr_substitution_abs_maybe subf b1)
                                (appr_substitution_abs_maybe subf b2)
      | Min (b1,b2)      -> min (appr_substitution_abs_maybe subf b1)
                                (appr_substitution_abs_maybe subf b2)
      | Abs b            -> abs (appr_substitution_abs_maybe subf b)
      | Pow (k,b)        -> Pow (k, appr_substitution_abs_maybe subf b))
      |> simplify_vars_nonnegative

    let appr_substition_abs_all subf =
      appr_substitution_abs_maybe (Option.Monad.return % subf)

    let appr_substitution_abs_one v b' =
      appr_substitution_abs_maybe (fun v' -> if v' = v then Some b' else None)

    let rec rename map =
      fold
        ~const:(fun c -> Const c)
        ~var:(fun v -> Var (RenameMap.find v map v))
        ~neg:(fun b -> Neg (rename map b))
        ~plus:(fun b1 b2 -> Sum (rename map b1, rename map b2))
        ~times:(fun b1 b2 -> Product (rename map b1, rename map b2))
        ~exp:(fun e b -> Pow(e, rename map b))
        ~max:(fun b1 b2 -> Max (rename map b1, rename map b2))
        ~min:(fun b1 b2 -> Min (rename map b1, rename map b2))
        ~abs:(fun b -> Abs (rename map b))
        ~inf:(Infinity)

    let degree n = raise (Failure "degree for MinMaxPolynomial not yet implemented")

    let eval p valuation = raise (Failure "eval for MinMaxPolynomial not yet implemented")
    let eval_f p valuation = raise (Failure "eval_f for MinMaxPolynomial not yet implemented")
    let of_string p = raise (Failure "of_string for MinMaxPolynomial not yet implemented")
    let coeff_of_var p= raise (Failure "coeff_of_var for MinMaxPolynomial not yet implemented")
    let of_coeff_list p= raise (Failure "of_coeff_list for MinMaxPolynomial not yet implemented")


  end
