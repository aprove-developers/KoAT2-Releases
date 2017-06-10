module Variables =
    struct 
        type variable = string
        
        type mapping = variable*variable
        
        let mk_var (name : string) =
            name

        let to_string ( var : variable ) = var

        let varlist_to_string (vars : variable list) =
            (String.concat ", " (List.map to_string  vars))

        let equal (var1 : variable)  (var2 : variable) = (var1 == var2)

        let to_z3 (ctx : Z3.context) (var : variable) =
            Z3.Arithmetic.Integer.mk_const ctx (Z3.Symbol.mk_string ctx var)
      
    end;;

(*The module "String" contains all information needed for the functor "Make"*)
(*Due to readability we use it instead of the module "Variables"*)

module VarMap = Map.Make ( String )

module Powers =
    struct

        type pow = Pow of Variables.variable * int
        
            let to_string ( power : pow ) =
                match power with
                    |Pow (var, n ) -> 
                        if n<=0 then "1" 
                        else String.concat "^" [(Variables.to_string var); (string_of_int n)]
            
            let to_z3 (ctx : Z3. context)  ( power : pow ) =
                match power with
                    |Pow (var, n ) -> Z3.Arithmetic.mk_power ctx ( Variables.to_z3 ctx var ) (Z3.Arithmetic.Integer.mk_numeral_i ctx n)
            let mk_pow_from_var (var : Variables.variable) (n : int) = Pow (var,n)

            let get_variable (power : pow) =
                match power with 
                    |Pow (var,n) -> var

            let get_degree (power : pow) =
                match power with 
                    |Pow (var,n) -> n
            
            let equal (power1 : pow) (power2 : pow) =
                match (power1, power2) with
                    |(Pow (var1,n1), Pow (var2, n2)) -> (Variables.equal var1 var2) && ( n1 == n2)
    end;;

(*A monomial is a product of powers of variables, the empty product is interpreted as the integer 1*)
module Monomials =
    struct

        type monomial = Powers.pow list

            let to_z3  (ctx : Z3.context) ( mon : monomial ) = if mon !=[] then Z3.Arithmetic.mk_mul ctx (List.map (Powers.to_z3 ctx) mon) else Z3.Arithmetic.Integer.mk_numeral_i ctx 1
              
            let rec mk_mon (input : (Variables.variable*int) list) =
                match input with
                    |[] -> []
                    |(var, n)::rest -> (Powers.mk_pow_from_var var n) :: (mk_mon rest)

            let get_variables (mon : monomial) = Tools.remove_dup (List.map Powers.get_variable mon) 

            let get_degree (mon : monomial) = List.fold_left (+) 0 (List.map Powers.get_degree mon)

            let get_degree_variable (var : Variables.variable) (mon : monomial) =
                let var_list = List.filter (fun x-> Variables.equal (Powers.get_variable x) var ) mon  in 
                    get_degree var_list  

            let delete_var (var: Variables.variable) (mon : monomial) =
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

    end;;      


module ScaledMonomials =
    struct
        type scaled_mon = Scaled of (Big_int.big_int * Monomials.monomial)
            
            let mk_scaled_mon_from_mon (coeff : Big_int.big_int) (mon:Monomials.monomial) = Scaled (coeff,mon)

            let to_z3 (ctx:Z3.context) (scaled : scaled_mon) =
                match scaled with
                    | Scaled (coeff, mon) -> Z3.Arithmetic.mk_mul ctx [(Z3.Arithmetic.Integer.mk_numeral_s ctx (Big_int.string_of_big_int coeff)) ; (Monomials.to_z3 ctx mon)]

            let get_coeff (scaled : scaled_mon) =
                match scaled with
                    | Scaled (coeff, mon) -> coeff

            let get_monom (scaled : scaled_mon) =
                match scaled with
                    | Scaled (coeff, mon) -> mon

            let simplify (scaled : scaled_mon) =
                match scaled with
                    | Scaled (coeff, mon)-> Scaled (coeff, (Monomials.simplify mon))

            let to_string_simplified (scaled : scaled_mon) =
                match scaled with
                    | Scaled (coeff, mon)-> 
                        if mon == [] then String.concat "" ["(" ; (Big_int.string_of_big_int coeff) ; ")"] 
                        else String.concat "" ["(" ; (Big_int.string_of_big_int coeff) ; ")" ; "*" ; (Monomials.to_string mon)]

            let to_string (scaled : scaled_mon) = to_string_simplified (simplify scaled)

            let equal (scaled1 : scaled_mon) (scaled2 : scaled_mon) =
                match (scaled1, scaled2) with
                    |(Scaled (coeff1, mon1), Scaled (coeff2, mon2)) -> (Big_int.eq_big_int coeff1 coeff2) && (Monomials.equal mon1 mon2)
 
    end;;

module Polynomials =
    struct
        (*A polynomial is a scaled sum of monomials, the coefficients are integers*)
        
        type polynomial = ScaledMonomials.scaled_mon list 

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

            let get_monomials (poly : polynomial) = List.map (ScaledMonomials.get_monom) poly

            let rec simplify_partial_simplified (poly : polynomial) =
                match poly with 
                    |[] -> []
                    |scaled::tail ->
                        let curr_monom = ScaledMonomials.get_monom scaled in
                            let curr_coeff = get_coeff curr_monom poly in
                                (ScaledMonomials.mk_scaled_mon_from_mon curr_coeff curr_monom) :: (simplify_partial_simplified (delete_monomial curr_monom tail) )

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
                         
                
     end;;
