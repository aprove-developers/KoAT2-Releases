
open Mapping

(*A polynomial is a scaled sum of monomials, the coefficients are integers*)
type polynomial = ScaledMonomials.scaled_mon list 

let get_degree (poly : polynomial) =
    Tools.max_of_int_list (List.map (ScaledMonomials.get_degree) poly )


let to_z3 (ctx : Z3.context) (poly : polynomial) = 
    if poly == [] then (Z3.Arithmetic.Integer.mk_numeral_i ctx 0)
    else    Z3.Arithmetic.mk_add ctx  (List.map (ScaledMonomials.to_z3 ctx) poly)

(* Returns the coefficient of a monomial *)
let get_coeff (mon : Monomials.monomial) (poly : polynomial) = 
    let mon_reduced_poly =(List.filter (fun scaled-> Monomials.equal (ScaledMonomials.get_monom scaled) mon) poly ) in
        let coeff_list = List.map (ScaledMonomials.get_coeff) mon_reduced_poly in
            List.fold_left (Big_int.add_big_int) Big_int.zero_big_int coeff_list

let delete_monomial (mon : Monomials.monomial) (poly : polynomial) =
    List.filter (fun x -> not (Monomials.equal (ScaledMonomials.get_monom x) mon)) poly

let rec simplify_partial_simplified (poly : polynomial) =
    match poly with 
        |[] -> []
        |scaled::tail ->
            let curr_monom = ScaledMonomials.get_monom scaled in
                let curr_coeff = get_coeff curr_monom poly in
                    if (Big_int.eq_big_int curr_coeff Big_int.zero_big_int) then (simplify_partial_simplified (delete_monomial curr_monom tail))

                    else (ScaledMonomials.mk_scaled_mon_from_mon curr_coeff curr_monom) :: (simplify_partial_simplified (delete_monomial curr_monom tail) )

let simplify (poly : polynomial) =
    simplify_partial_simplified (List.map (ScaledMonomials.simplify) poly)

let to_string_simplified (poly : polynomial) = 
    if (poly == []) then "0" 
    else 
        String.concat "+" (List.map ScaledMonomials.to_string poly)

let to_string (poly : polynomial) = to_string_simplified (simplify poly)

let rec equal_simplified (poly1 : polynomial) (poly2 : polynomial) =
    if(List.length poly1 == List.length poly2) then
        match poly1 with
            |[] -> true
            | scaled :: tail ->
                let curr_mon = ScaledMonomials.get_monom scaled in
                    let curr_coeff = ScaledMonomials.get_coeff scaled in
                        (Big_int.eq_big_int curr_coeff (get_coeff curr_mon poly2)) && equal_simplified tail (delete_monomial curr_mon poly2)
    else false

let equal (poly1 : polynomial) (poly2 : polynomial) = 
    equal_simplified (simplify poly1) (simplify poly2)


(* Returns the monomials of a polynomial without the empty monomial *)
let get_monomials (poly : polynomial) = List.filter (fun x -> x <>[]) (List.map (ScaledMonomials.get_monom) (simplify poly))

(* Returns a variable as a polynomial *)

let from_var (var : Variables.variable) =
    let pow = (Powers.mk_pow_from_var var 1) in
        let scaled_with_one = ScaledMonomials.mk_scaled_mon_from_mon (Big_int.big_int_of_int 1)  [pow] in
            [scaled_with_one]

(* Return "zero" as a polynomial *)

let zero = []

(* Return "one" as a polynomial *)

let one = 
    let const = [] in
        [ ScaledMonomials.mk_scaled_mon_from_mon (Big_int.big_int_of_int 1) const ]

(* Gets the constant *)
let get_constant (poly : polynomial ) = get_coeff [] poly

let from_constant (c : Big_int.big_int) =
    [(ScaledMonomials.mk_scaled_mon_from_mon c [])]


(* Returns the variables of a polynomial *)          
let get_variables (poly:polynomial) =
    let monomials_of_poly = get_monomials (simplify poly) in
        Tools.remove_dup (List.concat (List.map Monomials.get_variables monomials_of_poly))
        
(* Checks whether a polynomial is a single variable *)
let is_var (poly : polynomial) = 
    let monomials_of_poly = get_monomials (simplify poly) in
        if (List.length monomials_of_poly) == 1 then
            Monomials.is_univariate_linear_monomial (List.nth monomials_of_poly 0)
        else false 
(* Checks wheather a polynomial is a single variable plus a constant*)
let is_var_plus_constant (poly : polynomial) =
    let const_rem = delete_monomial [] poly in
        is_var const_rem

(* Checks whether a polynomial is a sum of variables plus a constant *)
let is_sum_of_vars_plus_constant (poly : polynomial) =
    let const_rem = delete_monomial [] poly in
    List.for_all (fun scaled -> (Big_int.eq_big_int (ScaledMonomials.get_coeff scaled ) Big_int.unit_big_int) && (Monomials.is_univariate_linear_monomial (ScaledMonomials.get_monom scaled))) const_rem

(* Checks whether a polynomial is a sum of variables plus a constant *)
let is_sum_of_vars_plus_constant (poly : polynomial) =
    let deg = get_degree poly in
        if deg == 1 then true else false       

(* Checks whether a polyomial is linear and contains just one active variable*)
let is_univariate_and_linear (poly : polynomial) =
    let deg = get_degree poly in
        if deg == 1 then
            let variables = get_variables poly in
                (List.length variables == 1)
        else false     
let is_const (poly : polynomial) =
    (get_degree poly <= 0) 

let is_linear = is_sum_of_vars_plus_constant 

(*renames the variables occuring inside a polynomial*) 

let rename_vars (varmapping : string VarMap.t) (poly : polynomial) =
    List.map (ScaledMonomials.rename_scaled_mon varmapping) poly

(*multiply a polynomial by a constant*)

let mult_with_const (const : Big_int.big_int) (poly : polynomial) =
    List.map (ScaledMonomials.mult_with_const const) poly

let negate (poly : polynomial) =
    mult_with_const (Big_int.minus_big_int Big_int.unit_big_int) poly

(*addition of two polynomials is just concatenation*)

let add (poly1 : polynomial) (poly2 : polynomial) =
    simplify (List.append poly1 poly2)

let add_list (pollist : polynomial list) =
    simplify (List.concat pollist) 

let subtract (poly1 : polynomial) (poly2 : polynomial) =
    add poly1 (negate poly2)

(*multiplication of two polynomials*)

let rec mult (poly1 : polynomial) (poly2 : polynomial) =
    match poly1 with
        |[] -> []
        |scaled :: tail ->  add (List.map(ScaledMonomials.mult scaled) poly2) (mult tail poly2)

let rec pow_poly (poly1 : polynomial)  (d : int) =
    if (d <= 0) then one
    else mult (pow_poly poly1 (d-1)) poly1

(*instantiates the variables in a polynomial with big ints*)

let instantiate_with_big_int (varmapping : Big_int.big_int VarMap.t) (poly : polynomial) = 
List.fold_left (Big_int.add_big_int) (Big_int.zero_big_int) (List.map (ScaledMonomials.instantiate_with_big_int varmapping) poly)
