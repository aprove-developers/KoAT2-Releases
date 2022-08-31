
open Batteries
open Polynomials
open ProgramTypes
open Atoms

module VarMap = Map.Make(Var)

type t = {
  poly : RationalPolynomial.t VarMap.t;
  inverse_poly :  RationalPolynomial.t VarMap.t;
}
let vars t :VarSet.t= fold (fun acc poly -> VarSet.union acc (RationalPolynomial.vars poly)) VarSet.empty (VarMap.values t.poly)

let vars_as_list t : VarSet.elt list = VarSet.to_list @@ vars t 

let poly_as_list t = List.map (fun var -> VarMap.find var t.poly) (VarSet.to_list (vars t))

let identity_aut = {poly = VarMap.empty; inverse_poly = VarMap.empty}

(** [vars polys inv_polys] returns an automorphism, but does not check whether it truely is a mathematical automorphism*)
let of_poly_list vars_list poly_list inv_poly_list: t = 
    {        poly = List.fold_left2 (fun map var poly -> VarMap.add var poly map) VarMap.empty vars_list poly_list;  
     inverse_poly = List.fold_left2 (fun map var poly -> VarMap.add var poly map) VarMap.empty vars_list inv_poly_list 
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

let apply polys t = apply_poly_transformation (vars_as_list t) polys (poly_as_list t)

let apply_inv polys t = apply_poly_transformation (vars_as_list t) polys (poly_as_list t)