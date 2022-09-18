open Batteries
open Polynomials
open ProgramTypes
open Atoms
open BoundsInst
open Formulas
open Constraints
open PolyExponential
open TransitionLabel

module Valuation = Valuation.Make(OurInt)
module VarMap = Map.Make(Var)
module ParameterMonomial = Monomials.Make(PolynomialOver(OurInt))

module Endomorphism =
  struct
  type t= {
    poly : ParameterPolynomial.t VarMap.t;
    inv_poly : ParameterPolynomial.t VarMap.t;
  }
  let to_string t = "Automorphism:\n" ^ VarMap.fold (fun var poly str -> str ^ (Var.to_string var)^" -> " ^ (ParameterPolynomial.to_string poly)^"\n") t.poly ""
                    ^"Inverse: \n"^      VarMap.fold (fun var poly str -> str ^ (Var.to_string var)^" -> " ^ (ParameterPolynomial.to_string poly)^"\n") t.inv_poly ""
  
  let vars t :VarSet.t= fold (fun acc poly -> VarSet.union acc (ParameterPolynomial.vars poly)) VarSet.empty (VarMap.values t.poly)
  
  let vars_as_list t : Var.t list =  List.of_enum @@ VarMap.keys t.poly

  let identity_end = {poly = VarMap.empty;
                  inv_poly = VarMap.empty}
  let poly_list t = List.map (fun var -> VarMap.find var t.poly) (vars_as_list t)
  
  let inv_poly_list t = List.map (fun var -> VarMap.find var t.inv_poly) (vars_as_list t)

  let poly_map t = t.poly 

  let inv_poly_map t = t.inv_poly 

  let degree_of_polylist polys = 
    ParameterPolynomial.degree @@ List.max ~cmp:(fun x y -> if ((ParameterPolynomial.degree x) < (ParameterPolynomial.degree y)) then -1 else 1) polys 

  let degree_of_endomorphism t =
    degree_of_polylist (poly_list t)

  (** Haskell's [m..n] is create_list m n, which is [m;m+1;...;n] or [] if m>n. Tail recursive*)
  let create_list i j = 
      let rec create_list_acc i j acc = 
        if (i<=j) then
          create_list_acc i (j-1) (j::acc)  
        else 
          acc
      in 
      create_list_acc i j []
      
      
  let rec enumerate_help d length = 
    match d, length with 
      | 0,_ -> [List.init length (fun x->0)]
      | _, 1 -> [[d]]
      | d, length -> 
        List.flatten @@ List.map (fun x -> List.map (fun xs -> x::xs) (enumerate_help (d-x) (length-1))) 
                                (create_list 0 d)
          

  (** enumerates all polynomials of degree less or equal than d with variables *)
  let enumerate_all_polys_degree d (vars:Var.t list)  = 
    match d with 
    | 0 -> [Polynomial.one]
    | _ -> let coeffs =  List.map (List.remove_at 1) (enumerate_help d ((List.length vars)+1)) in 
          List.map (fun xs -> List.fold (Polynomial.mul) Polynomial.one (List.map2 (fun var x -> Polynomial.of_power var x) vars xs)) coeffs 
  
  
   (*let create_default_mapping ?(letter_for_index="a") degree vars  = 
    let polys = enumerate_all_polys_degree degree vars (* get all monomials up to degree *)
                |> List.map ParameterPolynomial.of_polynomial  in 
    let inv_endomorphism_polys  = 
      List.map (fun var -> 
                List.fold (ParameterPolynomial.(+)) 
                          ParameterPolynomial.zero 
                          (List.map2 (fun int poly -> ParameterPolynomial.mult_with_const (Polynomial.of_var (Var.of_string (letter_for_index^(Var.to_string var)^(Int.to_string int)))) poly) 
                                    (create_list 1 (List.length polys)) 
                                    polys)) (*multiply each monomial with a distinct constant*)
      vars 
    in 
    List.fold_left2 (fun map var poly -> VarMap.add var poly map) VarMap.empty vars inv_endomorphism_polys *)

let create_default_mapping ?(letter_for_index="a") degree vars  = 
  (* we must be careful with storage usage, thats why we need a dense encoding of monomials, which is ensured by giving every monomial a number  *)
  let res = enumerate_all_polys_degree degree vars (* get all monomials up to degree *)
                |> List.map ParameterPolynomial.of_polynomial in 
  let polys =  fun var -> List.map2 (fun int poly -> ParameterPolynomial.mult_with_const (Polynomial.of_var (Var.of_string (letter_for_index^(Var.to_string var)^(Int.to_string int)))) poly) (create_list 1 (List.length res)) res  in 
  print_int (List.length res);
  let endomorphism_polys = 
      List.map (fun var -> 
                  (*multiply each monomial with a distinct constant*)
                List.fold (ParameterPolynomial.(+)) ParameterPolynomial.zero (polys var)
                ) 
                vars 
    in
    List.fold_left2 (fun map var poly -> VarMap.add var poly map) VarMap.empty vars endomorphism_polys

  (*let create_default_mapping ?(letter_for_index="a") degree vars  = 
    let endomorphism_polys = 
      List.map (fun var -> 
                let polys = enumerate_all_polys_degree degree vars (* get all monomials up to degree *)
                |> List.map ParameterPolynomial.of_polynomial 
                |> List.map (fun poly -> ParameterPolynomial.mult_with_const (Polynomial.of_var (Var.of_string (letter_for_index^(Var.to_string var)^(ParameterPolynomial.to_string poly)))) poly) in (*multiply each monomial with a distinct constant*)
                List.fold (ParameterPolynomial.(+)) ParameterPolynomial.zero polys
                ) 
                vars 
    in 
    List.fold_left2 (fun map var poly -> VarMap.add var poly map) VarMap.empty vars endomorphism_polys *)

  (** [vars polys inv_polys] returns an endomorphism*)
  let of_poly_list vars_list poly_list : t = 
    {        poly = List.fold_left2 (fun map var poly -> VarMap.add var poly map) VarMap.empty vars_list poly_list;    
            inv_poly = create_default_mapping (Int.pow (degree_of_polylist poly_list) ((List.length vars_list)-1)) vars_list 
    }

  let of_degree vars_list degree : t = 
    {   poly =     create_default_mapping (degree)                                     vars_list ~letter_for_index:("b") ;
        inv_poly = create_default_mapping (Int.pow degree ((List.length vars_list)-1)) vars_list 
    }


  let apply t (ppoly:ParameterPolynomial.t) = 
    ParameterPolynomial.substitute_all ((t.poly)) ppoly (*let x = VarMap.add  var ppoly VarMap.empty  in  (ParameterPolynomial.of_polynomial @@ RationalPolynomial.normalize (VarMap.find var (poly_map t)))*)

  let transform_update t update_map =
    List.map (ParameterPolynomial.substitute_all update_map) (poly_list t) 
    |> List.map ParameterPolynomial.simplify
    |> List.map (ParameterPolynomial.substitute_all (inv_poly_map t)) 
    |> List.map ParameterPolynomial.simplify (*this is done twice to avoid overflow *)

  let make_formula_help var sum_that_is_equal_to_var = 
    let (coeffs_of_var, other_coeffs) = ParameterPolynomial.monomials_with_coeffs sum_that_is_equal_to_var
                  |> List.partition (fun (x,y)-> ParameterPolynomial.equal (ParameterPolynomial.of_monomial y) (ParameterPolynomial.of_var var)) 
                  |> (fun (xs,ys) -> (List.map (fun (x,y) -> x) xs, List.map (fun (x,y) -> x) ys )) (*keep only the coefficients, not the monomials*)
    in
        List.map (fun x ->Formula.mk_eq x Polynomial.one) coeffs_of_var (*set the coefficients of var to 1 *)
      @ List.map (fun x ->Formula.mk_eq x Polynomial.zero) other_coeffs  (* set all other coefficients to 0 *)
              |> List.fold (Formula.mk_and) Formula.mk_true (* conjugate all formulas *)

  (**returns the formula to check right invertibility for one variable, it is technically not p_r_i as in Termination of Polynomial Loops but its later mentioned transformed version *)
  let get_p_r_i endomorphism vars var =  
    let sum_that_is_equal_to_var = ParameterPolynomial.simplify @@ apply endomorphism (VarMap.find var endomorphism.inv_poly) in 
    make_formula_help var sum_that_is_equal_to_var  
    
  let apply_inverse endomorphism poly = 
    ParameterPolynomial.substitute_all endomorphism.inv_poly poly 

  let get_p_l_i endomorphism vars var = 
    let sum_that_is_equal_to_var = ParameterPolynomial.simplify @@ apply_inverse endomorphism (VarMap.find var endomorphism.poly) in  
    make_formula_help var sum_that_is_equal_to_var  
   
  let formula_to_check_invertibility endomorphism = 
    let vars_as_list = vars_as_list endomorphism in
       List.map (get_p_r_i endomorphism vars_as_list) vars_as_list
     @ List.map (get_p_l_i endomorphism vars_as_list) vars_as_list
    |> List.fold (Formula.mk_and) Formula.mk_true
  
  let rec formula_to_check_twn_help vars (transformed_update:Polynomials.ParameterPolynomial.t list) = match vars, transformed_update with 
    | [], [] -> Formula.mk_true 
    | (var::vars), (transformed_update::transformed_updates) ->
  print_endline "endline131";
          let coeffs = ParameterPolynomial.monomials_with_coeffs transformed_update in 
          
  print_endline "endline133";
          let zero_coeffs = List.filter (fun  (coeff,monom) -> not (VarSet.subset (ParameterMonomial.vars monom) (VarSet.of_list vars) 
                                                          || (ParameterMonomial.equal (monom) (ParameterMonomial.of_var var)
                                                          && VarSet.subset (VarSet.of_list [var]) (ParameterMonomial.vars monom)))) coeffs 

                          |> List.map Tuple2.first in
  print_endline "endline139";
          List.map (fun x ->Formula.mk_eq x Polynomial.zero) zero_coeffs (* set all the coefficients of monomials we do not want to zero *)
          |> List.fold (Formula.mk_and) Formula.mk_true (* fold them together *)
          |> Formula.mk_and (formula_to_check_twn_help vars transformed_updates) 
    | _,_ -> raise (Invalid_argument "formula_to_check_twn: lengths of vars and transformed_updates do not match")


  let formula_to_check_twn vars endomorphism twn_update_map =
  print_endline "endline147"; 
    let polys = transform_update endomorphism @@ VarMap.map ParameterPolynomial.of_polynomial twn_update_map in
  print_endline "endline149"; 
    formula_to_check_twn_help vars polys 


end 
module Automorphism =
  struct 
type t = {
  poly : RationalPolynomial.t VarMap.t;
  inv_poly :  Polynomial.t VarMap.t;
}

let to_string t = "Automorphism:\n" ^ VarMap.fold (fun var poly str -> str ^ (Var.to_string var)^" -> " ^ (RationalPolynomial.to_string poly)^"\n") t.poly ""
                  ^"Inverse: \n"^      VarMap.fold (fun var poly str -> str ^ (Var.to_string var)^" -> " ^ (Polynomial.to_string poly)^"\n")         t.inv_poly ""
  

let vars t = VarSet.of_enum @@ VarMap.keys t.poly

let poly_map t = t.poly 

let inv_poly_map t = t.inv_poly 

let inv_rational_poly_map t = VarMap.map (RationalPolynomial.of_intpoly) t.inv_poly 

let vars_as_list t : Var.t list =  List.of_enum @@ VarMap.keys t.poly

let poly_list t = List.map (fun var -> VarMap.find var t.poly) (VarSet.to_list (vars t))

let inv_poly_list t = List.map (fun var -> VarMap.find var t.inv_poly) (VarSet.to_list (vars t))

let inv_rational_poly_list t = List.map RationalPolynomial.of_intpoly @@ inv_poly_list t

let identity_aut = {poly = VarMap.empty; inv_poly = VarMap.empty}

(** [vars polys inv_polys] returns an automorphism, but does not check whether it truely is a mathematical automorphism*)
let of_poly_list vars_list poly_list inv_poly_list: t = 
    {        poly = List.fold_left2 (fun map var poly -> VarMap.add var poly map) VarMap.empty vars_list poly_list;  
     inv_poly = List.fold_left2 (fun map var poly -> VarMap.add var poly map) VarMap.empty vars_list inv_poly_list 
    }


let compose_rational_polynomials variables new_polynomials old_poly = 
  let rec compose_polynomials_acc variables new_polynomials old_poly acc_poly = match variables, new_polynomials with 
    | [],[] -> RationalPolynomial.add old_poly acc_poly
    | x::xs,poly::polys -> compose_polynomials_acc xs polys old_poly
                                 (RationalPolynomial.add acc_poly @@ RationalPolynomial.sub (RationalPolynomial.substitute x ~replacement:poly old_poly) old_poly) 
    | _,_ -> (print_string "546"; raise Not_found ) (* TODO: find better exception for this case or better idea than exception *) in 
  compose_polynomials_acc variables new_polynomials old_poly (RationalPolynomial.zero) 

(** compose_int_polynomials [[x;y]] [[x^2;y^3]] [(x <- x+y)]  returns [x <- x^2+y^3]. 
    tail recursive*)
let compose_int_polynomials variables new_polynomials old_poly = 
  let rec compose_polynomials_acc variables new_polynomials old_poly acc_poly = match variables, new_polynomials with 
    | [],[] -> Polynomial.add old_poly acc_poly
    | x::xs,poly::polys -> compose_polynomials_acc xs polys old_poly
                                 (Polynomial.add acc_poly @@ Polynomial.sub (Polynomial.substitute x ~replacement:poly old_poly) old_poly) 
    | _,_ -> (print_string "547"; raise Not_found ) (* TODO: find better exception for this case or better idea than exception *) in 
  compose_polynomials_acc variables new_polynomials old_poly (Polynomial.zero) 

(*  *)
let apply_poly_transformation  variables old_polynomials new_polynomials = 
  List.map (compose_rational_polynomials variables new_polynomials) old_polynomials

let apply polys t = apply_poly_transformation (vars_as_list t) polys (poly_list t)

let apply_inv polys t = apply_poly_transformation (vars_as_list t) polys (poly_list t)

let transform_update t update_map =
  List.map (RationalPolynomial.substitute_all update_map) (poly_list t) 
                |> List.map (RationalPolynomial.substitute_all (inv_rational_poly_map t)) 
                |> List.map RationalPolynomial.normalize 

let transform_guard t guard = 
  Guard.map_polynomial (Polynomial.substitute_all t.inv_poly) guard 
(*helper function to transform_bound *)
let bound_map_of_automorphism t = VarMap.fold (fun key value map -> (VarMap.add key (BoundsInst.Bound.of_poly value) map)) (inv_poly_map t) VarMap.empty

(** applies the inverse automorphism to the bound*)
let transform_bound t bound = BoundsInst.Bound.substitute_all (bound_map_of_automorphism t) bound 

let of_endomorphism endomorphism (valuation:Polynomials.Polynomial.valuation) = 
    let inv_polys = List.map (ParameterPolynomial.eval_coefficients (fun x -> Valuation.eval x valuation)) @@ Endomorphism.inv_poly_list endomorphism in 
    let polys =     List.map (ParameterPolynomial.eval_coefficients (fun x -> Valuation.eval x valuation)) @@ Endomorphism.poly_list endomorphism in 
    let res = of_poly_list (Endomorphism.vars_as_list endomorphism) (List.map RationalPolynomial.of_intpoly polys) inv_polys in 
    Printf.printf "245 Automorphism: %s \n" @@ to_string res;
    res 

end