open! OurBase
open Polynomials

let logger = Logging.(get Bound)

module Make (Num : PolyTypes.OurNumber) = struct
  type valuation = Valuation.Make(Num).t
  type polynomial = PolynomialOver(Num).t
  type value = Num.t
  type indeterminate = Var.t

  module Poly = PolynomialOver (Num)

  (** Hide the internal workings of the type as far as possible.
      Only implement something here, if it requires modification of the internal structure *)
  module Repr : sig
    type 'a non_empty_list = 'a * 'a List.t [@@deriving eq, ord, sexp]

    type first_level_bound = private Var of Var.t | Log of Var.t | Exp of (bound * bound)
    and product_bound = private Product of { coeff : Num.t; mults : first_level_bound List.t }
    and bound = private Sum of product_bound non_empty_list [@@deriving eq, ord, sexp]

    val show_bound_inner : ?pretty:bool -> bound -> string
    val mk_var_first_level : Var.t -> first_level_bound
    val mk_log_first_level : Var.t -> first_level_bound
    val mk_singleton_product : first_level_bound -> product_bound
    val mk_singleton_sum : product_bound -> bound
    val to_constant_bound : bound -> Num.t Option.t
    val bound_of_constant : Num.t -> bound
    val zero_bound : bound
    val one_bound : bound
    val sum_bounds : bound Sequence.t -> bound
    val add_bound : bound -> bound -> bound
    val mul_bound : bound -> bound -> bound
    val product_bounds : bound Sequence.t -> bound
    val pow_bound : bound -> int -> bound
    val exp_bound : bound -> bound -> bound
  end = struct
    type 'a non_empty_list = 'a * 'a List.t [@@deriving eq, ord, sexp]

    type first_level_bound = Var of Var.t | Log of Var.t | Exp of (bound * bound)
    and multiplicands_bound = first_level_bound List.t
    and product_bound = Product of { coeff : Num.t; mults : first_level_bound List.t }
    and bound = Sum of product_bound non_empty_list [@@deriving eq, ord, sexp]

    let mk_var_first_level v = Var v
    let mk_log_first_level v = Log v
    let mk_singleton_product b = Product { coeff = Num.one; mults = [ b ] }
    let mk_singleton_sum b = Sum (b, [])

    (* helper modules for maps *)
    module MultiplicandsBoundComparator = struct
      module Inner = struct
        type t = multiplicands_bound [@@deriving eq, ord, sexp]
      end

      include Inner
      include Comparator.Make (Inner)
    end

    module BoundComparator = struct
      module Inner = struct
        type t = bound [@@deriving eq, ord, sexp]
      end

      include Inner
      include Comparator.Make (Inner)
    end

    let bound_of_constant c = Sum (Product { coeff = Num.abs c; mults = [] }, [])
    let zero_bound = bound_of_constant Num.zero
    let one_bound = bound_of_constant Num.one

    let to_constant_bound = function
      | Sum (Product { coeff; mults = [] }, []) -> Some coeff
      | _ -> None


    let to_first_level_bound = function
      | Sum (Product { coeff; mults = [ f ] }, []) when Num.(equal coeff one) -> Some f
      | _ -> None


    let to_var_bound b =
      match to_first_level_bound b with
      | Some (Var v) -> Some v
      | _ -> None


    let rec show_bound_inner ?(pretty = false) =
      let mul_sign =
        if pretty then
          "⋅"
        else
          "*"
      in
      let first_level = function
        | Var v -> Var.to_string ~pretty v
        | Log v -> "log(" ^ Var.to_string ~pretty v ^ ")"
        | Exp (b, e) ->
            let to_string_ t =
              match to_constant_bound t with
              | Some c -> Num.to_string c
              | None -> (
                  match to_var_bound t with
                  | Some v -> Var.to_string ~pretty v
                  | None -> "(" ^ show_bound_inner ~pretty t ^ ")")
            in
            to_string_ b ^ "^" ^ to_string_ e
      in
      let product = function
        | Product { coeff; mults = [] } -> Num.to_string coeff
        | Product { coeff; mults } ->
            let coeff_str =
              if Num.(equal coeff one) then
                ""
              else
                Num.to_string coeff ^ mul_sign
            in
            let mults_string = mults |> List.map ~f:first_level |> String.concat ~sep:mul_sign in
            coeff_str ^ mults_string
      in
      function
      | Sum (s, ss) -> s :: ss |> List.map ~f:product |> String.concat ~sep:" + "


    let sum_bounds bounds =
      let bounds = Sequence.to_list bounds in
      let execute () =
        let product_bounds = List.bind bounds ~f:(fun (Sum (s, ss)) -> s :: ss) |> Sequence.of_list in
        let summands_map =
          product_bounds
          |> Sequence.map ~f:(function Product { coeff; mults } -> (mults, coeff))
          |> Map.of_sequence_reduce (module MultiplicandsBoundComparator) ~f:Num.add
        in
        let not_zero = function
          | [], c when Num.(equal zero c) -> false
          | _ -> true
        in
        let combined_summands =
          Map.to_sequence ~order:`Increasing_key summands_map
          |> Sequence.filter ~f:not_zero
          |> Sequence.map ~f:(fun (mults, coeff) -> Product { coeff; mults })
          |> Sequence.to_list
        in
        match combined_summands with
        | p :: ps -> Sum (p, ps)
        | [] -> zero_bound
      in
      Logger.with_log logger Logger.DEBUG
        (fun () ->
          ("sum_bounds", [ ("bounds", Util.sequence_to_string ~f:show_bound_inner (Sequence.of_list bounds)) ]))
        ~result:show_bound_inner execute


    let add_bound b1 b2 = Sequence.append (Sequence.singleton b1) (Sequence.singleton b2) |> sum_bounds

    let mk_product_bound coeff mults =
      let mults =
        if Num.(equal coeff zero) then
          []
        else
          List.sort ~compare:compare_first_level_bound mults
      in
      Product { coeff; mults }


    let repeat_sequence x e = Sequence.init e ~f:(fun _ -> x)

    let rec pow_bound b e = repeat_sequence b e |> product_bounds

    (* We only call this recursively from exp_bound when we removed logs from the exponent (or via pow_bound for constant integral exponent) *)
    and exp_bound b e =
      let execute () =
        match (b, to_constant_bound b, to_constant_bound e) with
        (* double exp *)
        | Sum (Product { coeff; mults = [ Exp (b1, e1) ] }, []), _, _ when Num.(equal coeff one) ->
            exp_bound b1 (mul_bound e1 e)
        | _, Some c, _ when Num.(equal c zero) -> zero_bound
        | _, Some c, _ when Num.(equal c one) -> one_bound
        | _, _, Some c when Num.is_integral c -> pow_bound b (Num.to_int c)
        (* push in *)
        | Sum (Product { coeff; mults }, []), _, _ when List.length mults >= 2 ->
            exp_bound (bound_of_constant coeff) e
            :: List.map mults ~f:(fun m -> exp_bound (mk_singleton_sum @@ mk_singleton_product m) e)
            |> Sequence.of_list |> product_bounds
        | _, Some c, e_opt ->
            Option.bind e_opt ~f:(Option.map ~f:bound_of_constant % Num.root_pow c)
            |> Option.value_or_thunk ~default:(fun () ->
                let (Sum (s, ss)) = e in
                let log_vars, rem =
                  List.partition_map (s :: ss) ~f:(function
                    | Product { coeff; mults = [ Log v ] } when Num.(equal coeff one) -> Base.Either.First v
                    | r -> Base.Either.Second (mk_singleton_sum r))
                in
                if List.is_empty log_vars then
                  mk_singleton_sum (mk_singleton_product (Exp (b, e)))
                else
                  let rem_product_bounds =
                    let (Sum (s, ss)) = exp_bound b (sum_bounds (Sequence.of_list rem)) in
                    s :: ss
                  in
                  let log_part =
                    Sequence.of_list log_vars
                    |> Sequence.bind ~f:(fun v ->
                        if Num.(equal c one) then
                          Sequence.singleton (Var v)
                        else
                          repeat_sequence (Var v) (Num.log2int c))
                    |> Sequence.to_list
                  in
                  (* Now log_part should have no exps. So we multiply the bound "manually" back together here *)
                  Sequence.of_list rem_product_bounds
                  |> Sequence.map ~f:(fun (Product { coeff; mults }) ->
                      mk_product_bound coeff (log_part @ mults))
                  |> Sequence.map ~f:mk_singleton_sum |> sum_bounds)
        | _ -> mk_singleton_sum (mk_singleton_product (Exp (b, e)))
      in
      Logger.with_log logger Logger.DEBUG
        (fun () -> ("exp_bound", [ ("b", show_bound_inner b); ("e", show_bound_inner e) ]))
        ~result:show_bound_inner execute


    and mul_bound b1 b2 =
      let execute () =
        let addends = function
          | Sum (p, ps) -> p :: ps
        in
        (* simplify 2^(0.5) * 8^(0.5) -> (2*8)^0.5 -> 16^0.5 *)
        let mk_product_bound_and_simplify_exps coeff (multiplicands : first_level_bound list) =
          let const_exps, rem_exps, rem =
            let try_const_exp = function
              | Exp (b, e) ->
                  OptionMonad.liftM2
                    (fun c_b c_e -> `Fst (Tuple2.make c_b c_e))
                    (to_constant_bound b) (to_constant_bound e)
                  |> Option.value ~default:(`Snd (b, e))
              | Var v -> `Snd (mk_singleton_sum @@ mk_singleton_product (Var v), bound_of_constant Num.one)
              | m -> `Trd m
            in
            List.partition3_map multiplicands ~f:try_const_exp
          in
          let simplified_grouped_const_exps, non_simplified_consts_exp =
            Sequence.of_list const_exps |> Sequence.map ~f:Tuple2.swap
            |> Map.of_sequence_reduce (module Num) ~f:Num.mul
            |> Map.to_alist
            |> List.partition_map ~f:(fun (e, b) ->
                let try_compute_exp = Option.map (Num.root_pow b e) ~f:Either.first in
                Option.value try_compute_exp
                  ~default:(Either.second (bound_of_constant b, bound_of_constant e)))
          in

          let rem_exps_grouped_and_simplified =
            let rem_exps =
              Sequence.append (Sequence.of_list non_simplified_consts_exp) (Sequence.of_list rem_exps)
            in
            let map =
              Sequence.map rem_exps ~f:(fun (b, e) -> (b, [ e ]))
              |> Map.of_sequence_reduce (module BoundComparator) ~f:List.append
            in
            (* Either.t to indicate progress *)
            Map.fold map ~init:(Either.first []) ~f:(fun ~key:b ~data:es ->
                let reconstr_var_int_exp v c = Sequence.to_list (repeat_sequence (Var v) (Num.to_int c)) in
                match es with
                | [ e ] ->
                    let reconstruct_exp b e =
                      let opt =
                        let open OptionMonad in
                        let* v = to_var_bound b in
                        let* c = to_constant_bound e in
                        Option.some_if (Num.is_integral c) (reconstr_var_int_exp v c)
                      in
                      Option.value opt ~default:[ Exp (b, e) ]
                    in
                    Either.First.map ~f:(List.append (reconstruct_exp b e))
                | es -> begin
                    let sum_es = sum_bounds (Sequence.of_list es) in
                    let as_exped () = exp_bound b sum_es in
                    function
                    | Either.First l -> (
                        match (to_var_bound b, to_constant_bound sum_es) with
                        | Some v, Some c_e when Num.is_integral c_e ->
                            Either.first (reconstr_var_int_exp v c_e @ l)
                        | _ ->
                            Either.Second
                              (as_exped () :: List.map ~f:(mk_singleton_sum % mk_singleton_product) l))
                    | Either.Second l -> Either.Second (as_exped () :: l)
                  end)
          in

          let coeff_from_simpl_const =
            Num.mul coeff (List.fold ~init:Num.one ~f:Num.mul simplified_grouped_const_exps)
          in
          match rem_exps_grouped_and_simplified with
          | Either.First l -> mk_singleton_sum @@ mk_product_bound coeff_from_simpl_const (l @ rem)
          | Either.Second l ->
              let rem_bounds =
                Sequence.map (Sequence.of_list rem) ~f:(mk_singleton_sum % mk_singleton_product)
              in
              let bounds_from_exps = Sequence.of_list (bound_of_constant coeff_from_simpl_const :: l) in
              Sequence.append bounds_from_exps rem_bounds |> product_bounds
        in

        Sequence.cartesian_product (Sequence.of_list @@ addends b1) (Sequence.of_list @@ addends b2)
        |> Sequence.map ~f:(function Product p1, Product p2 ->
            mk_product_bound_and_simplify_exps (Num.mul p1.coeff p2.coeff) (p1.mults @ p2.mults))
        (* |> Sequence.map ~f:mk_singleton_sum *) |> sum_bounds
      in
      Logger.with_log logger Logger.DEBUG
        (fun () -> ("mul_bound", [ ("b1", show_bound_inner b1); ("b2", show_bound_inner b2) ]))
        ~result:show_bound_inner execute


    and product_bounds bounds = Sequence.reduce ~f:mul_bound bounds |> Option.value ~default:one_bound
  end

  include Repr

  type t = bound Option.t [@@deriving eq, ord, sexp]

  let prove_finiteness = identity
  let bound_of_var v = mk_singleton_sum (mk_singleton_product (mk_var_first_level v))
  let of_var = OptionMonad.return % bound_of_var
  let of_constant = OptionMonad.return % bound_of_constant
  let zero = OptionMonad.return zero_bound
  let one = OptionMonad.return one_bound
  let of_OurInt = OptionMonad.return % bound_of_constant % Num.of_ourint
  let to_constant = Option.bind ~f:to_constant_bound
  let is_constant_bound = Option.is_some % to_constant_bound
  let is_constant = Option.value_map ~default:false ~f:is_constant_bound

  let rec fold_bound ~const ~var ~plus ~times ~exp ~log p =
    let fold_ = fold_bound ~const ~var ~plus ~times ~exp ~log in
    let first_level = function
      | Var v -> var v
      | Log v -> log v
      | Exp (b, e) -> exp (fold_ b) (fold_ e)
    in
    let product = function
      | Product { coeff; mults } ->
          Sequence.map ~f:first_level (Sequence.of_list mults) |> Sequence.fold ~init:(const coeff) ~f:times
    in
    match p with
    | Sum (b, bs) -> Sequence.of_list bs |> Sequence.map ~f:product |> Sequence.fold ~init:(product b) ~f:plus


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
    (* | LogarithmicPolynomial (x, y) when OurRational.(equal x one && equal y zero) ->
        "WORST_CASE(?, O(log(n)))" *)
    | LogarithmicPolynomial (x, y) -> "WORST_CASE(?, O(n^" ^ OurRational.(to_string (x + y)) ^ "))"
    | Exponential _ -> "WORST_CASE(?, FINITE)"


  let show_complexity_termcomp_log = function
    | Inf -> "MAYBE"
    | LogarithmicPolynomial (x, y) when OurRational.(equal x zero && equal y zero) -> "WORST_CASE(?, O(1))"
    | LogarithmicPolynomial (x, y) when OurRational.(equal x zero) ->
        "WORST_CASE(?, O(" ^ correct_str "n" y ^ "))"
    | LogarithmicPolynomial (x, y) when OurRational.(equal y zero) ->
        "WORST_CASE(?, O(" ^ correct_str "log(n)" x ^ "))"
    | LogarithmicPolynomial (x, y) ->
        "WORST_CASE(?, O(" ^ correct_str "log(n)" x ^ "*" ^ correct_str "n" y ^ "))"
    | Exponential _ -> "WORST_CASE(?, FINITE)"


  let lex_compare (b1, a1) (b2, a2) =
    if OurRational.equal b1 b2 && OurRational.equal a1 a2 then
      0
    else if OurRational.(b1 > b2) || OurRational.(b1 = b2 && a1 > a2) then
      1
    else
      -1


  let asymptotic_complexity =
    let rec first_level = function
      | Var _ -> LogarithmicPolynomial (OurRational.zero, OurRational.one)
      | Log _ -> LogarithmicPolynomial (OurRational.one, OurRational.zero)
      | Exp (b, e) -> (
          match to_constant_bound e with
          | Some c -> (
              let c_our_rational = Num.to_our_rational c in
              match sum_bound b with
              | LogarithmicPolynomial (x, y) ->
                  LogarithmicPolynomial (OurRational.(x * c_our_rational), OurRational.(y * c_our_rational))
              | Exponential _ as e -> e
              | Inf -> Inf)
          | None -> (
              match sum_bound e with
              | Inf -> Inf
              | LogarithmicPolynomial _ -> Exponential 1
              | Exponential x -> Exponential (Int.succ x)))
    and product_bound (Product { coeff; mults }) =
      let f x y =
        match (x, y) with
        | Inf, _ -> Inf
        | _, Inf -> Inf
        | LogarithmicPolynomial (x1, y1), LogarithmicPolynomial (x2, y2) ->
            LogarithmicPolynomial (OurRational.(x1 + x2), OurRational.(y1 + y2))
        | Exponential x, Exponential y -> Exponential (Int.max x y)
        | LogarithmicPolynomial _, Exponential y -> Exponential y
        | Exponential x, LogarithmicPolynomial _ -> Exponential x
      in
      Sequence.map ~f:first_level (Sequence.of_list mults)
      |> Sequence.fold ~init:OurRational.(LogarithmicPolynomial (zero, zero)) ~f
    and sum_bound (Sum (s, ss)) =
      let f x y =
        match (x, y) with
        | Inf, _ -> Inf
        | _, Inf -> Inf
        | LogarithmicPolynomial (x1, y1), LogarithmicPolynomial (x2, y2) ->
            if lex_compare (y1, x1) (y2, x2) == -1 then
              LogarithmicPolynomial (x2, y2)
            else
              LogarithmicPolynomial (x1, y1)
        | Exponential x, Exponential y -> Exponential (Int.max x y)
        | LogarithmicPolynomial _, Exponential y -> Exponential y
        | Exponential x, LogarithmicPolynomial _ -> Exponential x
      in
      Sequence.of_list ss |> Sequence.map ~f:product_bound |> Sequence.fold ~f ~init:(product_bound s)
    in
    Option.value_map ~f:sum_bound ~default:Inf


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


  let show_bound ?(pretty = false) t =
    match t with
    | Some b -> show_bound_inner ~pretty b
    | None ->
        if pretty then
          "∞"
        else
          "inf"


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
      match (to_constant_bound b1, to_constant_bound b2) with
      | Some c1, Some c2 when Num.Compare.(c1 > c2) -> Some true
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
            match
              (to_constant_bound b1, to_constant_bound b2)
            with
            | Some c1, Some c2 when Num.Compare.(c1 >= c2) -> Some true
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
  let sum_list = Option.map ~f:(sum_bounds % Sequence.of_list) % OptionMonad.sequence
  let sum = sum_list % Sequence.to_list
  let add = OptionMonad.liftM2 add_bound

  let max b1 b2 =
    match (to_constant b1, to_constant b2) with
    | Some c1, Some c2 -> of_constant (Num.max c1 c2)
    | _, _ -> add b1 b2 (* TODO Improve this. *)


  let maximum bounds = Sequence.reduce ~f:max bounds |? zero

  let mul b1 b2 =
    match (Option.map ~f:to_constant_bound b1, Option.map ~f:to_constant_bound b2) with
    (* 0 * inf = inf * 0 = 0 *)
    | Some (Some c), None ->
        if Num.equal c Num.zero then
          zero
        else
          None
    | None, Some (Some c) ->
        if Num.equal c Num.zero then
          zero
        else
          None
    | _ -> OptionMonad.liftM2 mul_bound b1 b2


  let product bounds = Sequence.reduce ~f:mul bounds |? one
  let pow bound n = Option.map ~f:(flip pow_bound n) bound

  (** Addition of two elements. *)
  let ( + ) = add

  (** Multiplication of two elements *)
  let ( * ) = mul

  (** Raises an element to the power of an integer value. *)
  let ( ** ) = pow

  let exp = OptionMonad.liftM2 exp_bound
  let exp_int value = Option.map ~f:(exp_bound (bound_of_constant @@ Num.of_ourint value))
  let infinity = None

  let of_poly =
    OptionMonad.return
    % Poly.fold ~const:bound_of_constant ~indeterminate:bound_of_var ~plus:add_bound ~times:mul_bound
        ~pow:pow_bound


  let of_intpoly =
    OptionMonad.return
    % Polynomial.fold ~const:(bound_of_constant % Num.of_ourint) ~indeterminate:bound_of_var ~plus:add_bound
        ~times:mul_bound ~pow:pow_bound


  let log_bound v = mk_singleton_sum (mk_singleton_product (mk_log_first_level v))
  let log = Option.some % log_bound
  let log_of_constant_bound = bound_of_constant % Num.log
  let log_of_constant = Option.some % log_of_constant_bound

  let log_of_bound =
    let rec first_level = function
      | Var v -> log_bound v
      | Log v -> log_bound v
      | Exp (b, e) -> mul_bound (sum_level b) e
    and prod_level = function
      | Product { coeff; mults } ->
          let mults_rec = List.map ~f:first_level mults in
          log_of_constant_bound coeff :: mults_rec |> sum_bounds % Sequence.of_list
    and sum_level = function
      | Sum (s, ss) -> Sequence.of_list (s :: ss) |> Sequence.map ~f:prod_level |> sum_bounds
    in
    Option.map ~f:sum_level


  let log_of_poly p =
    let of_poly = of_intpoly p in
    log_of_bound of_poly


  let bound_of_int i = bound_of_constant (Num.of_int @@ Int.abs i)
  let of_int = OptionMonad.return % bound_of_int
  let bound_of_var_string str = bound_of_var (Var.of_string str)
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


  let vars_bound =
    fold_bound ~const:(const VarSet.empty) ~var:VarSet.singleton ~plus:Set.union ~times:Set.union
      ~exp:Set.union ~log:VarSet.singleton


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


  let degree n = raise (Failure "degree for Bounds not yet implemented")
  let rename map p = raise (Failure "rename for Bounds not yet implemented")
  let eval p valuation = raise (Failure "eval for Bounds not yet implemented")
  let eval_f p valuation = raise (Failure "eval_f for Bounds not yet implemented")
  let coeff_of_var p = raise (Failure "coeff_of_var for Bounds not yet implemented")
  let of_coeff_list p = raise (Failure "of_coeff_list for Bounds not yet implemented")
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


  module FirstLevelBoundComparator = struct
    module Inner = struct
      type t = first_level_bound [@@deriving eq, ord, sexp]
    end

    include Inner
    include Comparator.Make (Inner)
  end

  let mth_root_bound m (Sum (s, ss)) =
    let first_level_with_exp (f_l_b, occ) =
      let b = mk_singleton_sum (mk_singleton_product f_l_b) in
      let occ_divm_bound = bound_of_constant OurRational.(of_ourint occ / of_int m) in
      match f_l_b with
      | Var _ -> exp_bound b occ_divm_bound
      | Log _ -> exp_bound b occ_divm_bound
      | Exp (b, e) -> exp_bound b (mul_bound e @@ occ_divm_bound)
    in
    let product_level (Product { coeff; mults }) =
      let new_coeff_bound = bound_of_constant (OurRational.mth_root_up m coeff) in
      let new_mults =
        Sequence.of_list mults
        |> Sequence.map ~f:(fun f_l_b -> (f_l_b, OurInt.one))
        |> Map.of_sequence_reduce (module FirstLevelBoundComparator) ~f:OurInt.add
        |> Map.to_sequence |> Sequence.map ~f:first_level_with_exp |> Sequence.to_list
      in
      new_coeff_bound :: new_mults |> product_bounds % Sequence.of_list
    in
    Sequence.of_list (s :: ss) |> Sequence.map ~f:product_level |> sum_bounds


  let mth_root m =
    if Int.equal m 1 then
      identity
    else
      Option.map ~f:(mth_root_bound m)


  let sqrt = mth_root 2
end

module BinaryBound = struct
  type t = Finite | Infinite [@@deriving eq, ord, sexp]
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
