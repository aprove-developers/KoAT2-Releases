open Batteries
open Polynomials
open Atoms
open Constraints

(** This module provides functions which convert koat datastructures to equivalent apron datastructures. *)
module Koat2Apron =
  struct

    (** Converts a koat variable to its apron equivalent. *)
    let var_to_apron: Var.t -> Apron.Var.t =
      Apron.Var.of_string % Var.to_string

    (** Converts a koat variable set to its apron equivalent. *)
    let vars_to_apron: VarSet.t -> Apron.Var.t array =
      VarSet.map_to_array var_to_apron

    (** Converts a koat integer to its apron equivalent. *)
    (** TODO Usage of OurInt.to_int breaks usage of big_int *)
    let const_to_apron: OurInt.t -> Apron.Coeff.t =
      Apron.Coeff.s_of_int % OurInt.to_int

    (** Converts a koat polynomial to its apron equivalent. *)
    let poly_to_apron (environment: Apron.Environment.t) (poly: Polynomial.t): Apron.Texpr1.t =
      (* TODO Texpr1.Up: Round in which direction? *)
      let open Apron in
      Polynomial.fold
        ~const:(fun c ->
          Texpr1.Cst (const_to_apron c)
        )
        ~indeterminate:(fun v ->
          Texpr1.Var (var_to_apron v)
        )
        ~neg:(fun expr ->
          Texpr1.Unop (Texpr1.Neg, expr, Texpr1.Int, Texpr1.Up)
        )
        ~plus:(fun expr1 expr2 ->
          Texpr1.Binop (Texpr1.Add, expr1, expr2, Texpr1.Int, Texpr1.Up)
        )
        ~times:(fun expr1 expr2 ->
          Texpr1.Binop (Texpr1.Mul, expr1, expr2, Texpr1.Int, Texpr1.Up)
        )
        ~pow:(fun expr n ->
          Enum.repeat ~times:n expr
          |> Enum.fold
               (fun result expr -> Texpr1.Binop (Texpr1.Mul, result, expr, Texpr1.Int, Texpr1.Up))
               (Texpr1.Cst (const_to_apron OurInt.one))
        )
        poly
      |> Apron.Texpr1.of_expr environment

    (** Converts a koat atom to its apron equivalent *)
    let atom_to_apron (environment: Apron.Environment.t) (atom: Atom.t): Apron.Tcons1.t =
      let open Apron in
      Atom.fold
        ~subject:(poly_to_apron environment)
        (* We have a<=b, Apron uses form c>=0, therefore we need 0<=b-a *)
        ~le:(fun expr1 expr2 -> Tcons1.make (Texpr1.binop Texpr1.Sub expr2 expr1 Texpr1.Int Texpr1.Up) Tcons1.SUPEQ)
        ~lt:(fun expr1 expr2 -> Tcons1.make (Texpr1.binop Texpr1.Sub expr2 expr1 Texpr1.Int Texpr1.Up) Tcons1.SUP)
        atom

    (** Converts a koat constraint to its apron equivalent *)
    let constraint_to_apron (environment: Apron.Environment.t) (constr: Constraint.t): Apron.Tcons1.earray =
      let open Apron in
      let single_constraints =
        constr
        |> Constraint.atom_list
        |> List.map (atom_to_apron environment)
        |> Array.of_list (* Pretty dump, but more readable *)
      in
      let constraints =
        Tcons1.array_make environment (Array.length single_constraints)
      in
      (* Copy the array to an apron array, this loop seems to be the only way *)
      for i=0 to Array.length single_constraints - 1 do
        Tcons1.array_set constraints i (Array.get single_constraints i)
      done;
      constraints

  end

(** This module provides functions which convert apron datastructures to equivalent koat datastructures. *)
module Apron2Koat =
  struct

    (** Converts an apron variable to its koat equivalent. *)
    let var_from_apron: Apron.Var.t -> Var.t =
      Var.of_string % Apron.Var.to_string

    (** Converts an apron variable set to its koat equivalent. *)
    let vars_from_apron: Apron.Var.t array -> VarSet.t =
      VarSet.of_enum % Array.enum % Array.map var_from_apron

    (** Converts an apron integer to its koat equivalent. *)
    (** TODO Usage of OurInt.to_int breaks usage of big_int *)
    let const_from_apron (coeff: Apron.Coeff.t): OurInt.t =
      let open Apron in
      match coeff with
      | Coeff.Scalar (Scalar.Float float) ->
         OurInt.of_int (int_of_float float)
      | Coeff.Scalar (Scalar.Mpqf float) -> (OurInt.of_int % int_of_float % Mpqf.to_float) float
      | Coeff.Scalar (Scalar.Mpfrf float) -> (OurInt.of_int % int_of_float % Mpfrf.to_float) float
      | Coeff.Interval _ -> raise (Failure "Apron Intervals are not allowed!")

    (** Converts an apron polynomial to its koat equivalent. *)
    let poly_from_apron (expr: Apron.Texpr1.t): Polynomial.t =
      let open Apron in
      let rec expr_to_poly = function
        | Texpr1.Cst coeff -> Polynomial.of_constant (const_from_apron coeff)
        | Texpr1.Var var -> Polynomial.of_var (var_from_apron var)
        | Texpr1.Unop (Texpr1.Neg, expr, Texpr1.Int, _) -> Polynomial.neg (expr_to_poly expr)
        | Texpr1.Unop _ -> raise (Failure "Usage of not supported apron unary operator!")
        | Texpr1.Binop (Texpr1.Add, expr1, expr2, _, _) -> Polynomial.add (expr_to_poly expr1) (expr_to_poly expr2)
        | Texpr1.Binop (Texpr1.Sub, expr1, expr2, _, _) -> Polynomial.sub (expr_to_poly expr1) (expr_to_poly expr2)
        | Texpr1.Binop (Texpr1.Mul, expr1, expr2, _, _) -> Polynomial.mul (expr_to_poly expr1) (expr_to_poly expr2)
        | Texpr1.Binop (op,expr1,expr2,typ,_) ->
           raise (Failure ("Usage of not supported apron binary operator " ^ Texpr0.string_of_binop op ^ " with types " ^ Texpr0.string_of_typ typ ^ "!"))
      in
      expr
      |> Apron.Texpr1.to_expr
      |> expr_to_poly

    (** Converts an apron atom to its koat equivalent *)
    let atom_from_apron (single_constraint: Apron.Tcons1.t): Constraint.t =
      let open Apron in
      let comparator = match Tcons1.get_typ single_constraint with
      | Tcons1.EQ -> Constraint.mk_eq
      | Tcons1.SUPEQ -> Constraint.mk_ge
      | Tcons1.SUP -> Constraint.mk_gt
      | Tcons1.DISEQ -> raise (Failure "Usage of not supported apron disequality!")
      | Tcons1.EQMOD _ -> raise (Failure "Usage of not supported apron modequality!")
      in
      comparator (poly_from_apron (Tcons1.get_texpr1 single_constraint)) Polynomial.zero

    (** Converts a koat constraint to its apron equivalent *)
    let constraint_from_apron (constraint_array: Apron.Tcons1.earray): Constraint.t =
      (* If someone still needs a reason to switch to functional programming, here it is. *)
      let result = ref [] in
      for i=0 to Apron.Tcons1.array_length constraint_array - 1 do
        result := atom_from_apron (Apron.Tcons1.array_get constraint_array i)::!result
      done;
      Constraint.all !result
  end

let abstract_to_string a = Apron.Abstract1.print Format.str_formatter a; Format.flush_str_formatter ()
