open Mapping

type monomial = Powers.t list

type value = Big_int.big_int

let rec mk_mon (input : (Variables.t*int) list) =
    match input with
        |[] -> []
        |(var, n)::rest -> (Powers.mk_pow_from_var var n) :: (mk_mon rest)

let get_variables (mon : monomial) = Tools.remove_dup (List.map Powers.get_variable mon) 

let get_degree (mon : monomial) = List.fold_left (+) 0 (List.map Powers.get_degree mon)

let get_degree_variable (var : Variables.t) (mon : monomial) =
    let var_list = List.filter (fun x-> Variables.equal (Powers.get_variable x) var ) mon  in 
        get_degree var_list  

let delete_var (var: Variables.t) (mon : monomial) =
    List.filter(fun x -> let var_x = Powers.get_variable x in not (Variables.equal var var_x)) mon

let rec simplify (mon : monomial) =
    match mon with
        |[] -> []
        |power :: tail -> 
            let curr_var = (Powers.get_variable power) in
                let curr_deg = get_degree_variable curr_var mon in
                    if (curr_deg > 0) then 
                        let new_pow = (Powers.mk_pow_from_var curr_var curr_deg) in
                            new_pow :: simplify (delete_var curr_var tail)
                    else simplify (delete_var curr_var tail)
                
let to_string_simplified (mon : monomial) =
    if mon == [] then "1"
    else  String.concat "*" (List.map Powers.to_string mon)

let to_string (mon : monomial) = to_string_simplified (simplify mon)

let to_z3_simplified (ctx : Z3.context) ( mon : monomial ) = 
    if mon !=[] then Z3.Arithmetic.mk_mul ctx (List.map (Powers.to_z3 ctx) mon) 
    else Z3.Arithmetic.Integer.mk_numeral_i ctx 1
    
let to_z3 (ctx : Z3.context) ( mon : monomial ) = 
    to_z3_simplified ctx (simplify mon)

(*compares two monomials under the assumption that both have already been simplified*)
let rec equal_simplified (mon1 : monomial) (mon2 : monomial) =
        if (List.length mon1 == List.length mon2) then
            match mon1 with
            |[] -> true (*same length, hence mon2 == []*)
            |pow1::tail1 -> 
                let var1 = Powers.get_variable pow1 in
                    ((get_degree_variable var1 mon2) == (Powers.get_degree pow1)) && (equal_simplified tail1 (delete_var var1 mon2))
            
        else false

let equal (mon1 : monomial) (mon2 : monomial) = equal_simplified (simplify mon1)(simplify mon2)


let is_univariate_linear_monomial (mon : monomial) =
let variables_of_mon = (get_variables mon) in
    if (List.length variables_of_mon == 1) then
        ((get_degree_variable (List.nth variables_of_mon 0) mon) == 1)
    else false

let rename_monomial (varmapping : string VarMap.t) (mon : monomial) = 
List.map (Powers.rename_power varmapping) mon

(*Multiplication of monomials*)

let mult (mon1 : monomial) (mon2 : monomial) =
simplify (List.append mon1 mon2)  

let eval (varmapping : value VarMap.t) (mon : monomial) = 
List.fold_left (Big_int.mult_big_int) (Big_int.unit_big_int) (List.map (Powers.eval varmapping) mon)
