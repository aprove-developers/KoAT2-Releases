module Variables =
    struct 
        type variable = Var of string
        
        let mk_var (name : string) =
            Var name

        let to_string ( var : variable ) =
            match var with
                |Var str -> str
        let varlist_to_string (vars : variable list) =
            (String.concat ", " (List.map to_string  vars))

        let equals (var1 : variable)  (var2 : variable) =
            match (var1, var2) with
                | (Var name1, Var name2) -> name1 == name2

        let to_z3 (ctx : Z3.context) (var : variable) =
            match var with
                | Var name -> Z3.Arithmetic.Integer.mk_const ctx (Z3.Symbol.mk_string ctx name)
        
        let equals (var1 : variable) (var2 : variable) =
            match (var1, var2) with
                |(Var name1, Var name2)-> (name1==name2)

            
    end;;

module Powers =
    struct

        type pow = Pow of Variables.variable * int
        
            let to_string ( power : pow ) =
                match power with
                    |Pow (var, n ) -> String.concat "^" [(Variables.to_string var); (string_of_int n)]
            
            let to_z3 (ctx : Z3. context)  ( power : pow ) =
                match power with
                    |Pow (var, n ) -> Z3.Arithmetic.mk_power ctx ( Variables.to_z3 ctx var ) (Z3.Arithmetic.Integer.mk_numeral_i ctx n)
            let mk_pow_from_var (var : Variables.variable) (n : int) = Pow (var,n)

            let mk_pow_from_string (name : string) (n : int) = Pow ((Variables.mk_var name), n)

            let get_variable (power : pow) =
                match power with 
                    |Pow (var,n) -> var

            let get_degree (power : pow) =
                match power with 
                    |Pow (var,n) -> n
            
            let equals (power1 : pow) (power2 : pow) =
                match (power1, power2) with
                    |(Pow (var1,n1), Pow (var2, n2)) -> (Variables.equals var1 var2) && ( n1 == n2)
    end;;

(*A monomial is a product of powers of variables*)
module Monomials =
    struct

        type monomial = Powers.pow list

            let to_string ( mon : monomial ) = String.concat "*" (List.map Powers.to_string mon)

            let to_z3  (ctx : Z3.context) ( mon : monomial ) = if mon !=[] then Z3.Arithmetic.mk_mul ctx (List.map (Powers.to_z3 ctx) mon) else Z3.Arithmetic.Integer.mk_numeral_i ctx 1
              
            let rec mk_mon (input : (string*int) list) =
                match input with
                    |[] -> []
                    |(name, n)::rest -> (Powers.mk_pow_from_string name n) :: (mk_mon rest)

            let get_variables (mon : monomial) = Tools.remove_dup (List.map Powers.get_variable mon) 

            let get_degree (mon : monomial) = List.fold_left (+) 0 (List.map Powers.get_degree mon)

            let get_degree_variable (var : Variables.variable) (mon : monomial) =
                let var_list = List.filter (fun x-> Variables.equals (Powers.get_variable x) var ) mon  in 
                    get_degree var_list  

            let delete_var (var: Variables.variable) (mon : monomial) =
                List.filter(fun x -> let var_x = Powers.get_variable x in not (Variables.equals var var_x)) mon
  
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

            (*compares two monomials under the assumption that both have already been simplified*)
            let rec equals_simplified (mon1 : monomial) (mon2 : monomial) =
                    if (List.length mon1 == List.length mon2) then
                        match mon1 with
                        |[] -> true (*same length, hence mon2 == []*)
                        |pow1::tail1 -> 
                         let var1 = Powers.get_variable pow1 in
                             ((get_degree_variable var1 mon2) == (Powers.get_degree pow1)) && (equals_simplified tail1 (delete_var var1 mon2))
                        
                    else false
    
           let equals (mon1 : monomial) (mon2 : monomial) = equals_simplified (simplify mon1)(simplify mon2)
    end;;      


module ScaledMonomials =
    struct
        type scaledMon = Scaled of (Big_int.big_int * Monomials.monomial)

            let to_string (scaled : scaledMon) =
                match scaled with
                    | Scaled (coeff, mon)-> String.concat "*" [ (Big_int.string_of_big_int coeff) ; (Monomials.to_string mon)]
            
            let mk_scaledMon_from_mon (coeff : Big_int.big_int) (mon:Monomials.monomial) = Scaled (coeff,mon)

            let to_z3 (ctx:Z3.context) (scaled : scaledMon) =
                match scaled with
                    | Scaled (coeff, mon) -> Z3.Arithmetic.mk_mul ctx [(Z3.Arithmetic.Integer.mk_numeral_s ctx (Big_int.string_of_big_int coeff)) ; (Monomials.to_z3 ctx mon)]

            let get_coeff (scaled : scaledMon) =
                match scaled with
                    | Scaled (coeff, mon) -> coeff

            let get_monom (scaled : scaledMon) =
                match scaled with
                    | Scaled (coeff, mon) -> mon

            let simplify (scaled : scaledMon) =
                match scaled with
                    | Scaled (coeff, mon)-> Scaled (coeff, (Monomials.simplify mon))

            let equals (scaled1 : scaledMon) (scaled2 : scaledMon) =
                match (scaled1, scaled2) with
                    |(Scaled (coeff1, mon1), Scaled (coeff2, mon2)) -> (Big_int.eq_big_int coeff1 coeff2) && (Monomials.equals mon1 mon2)
 
    end;;

module Polynomials =
    struct
        (*A polynomial is a scaled sum of monomials, the coefficients are integers*)
        
        type polynomial = ScaledMonomials.scaledMon list 
 
            let to_string (poly : polynomial) = 
                if (poly == []) then "0" 
                else 
                    String.concat "+" (List.map ScaledMonomials.to_string poly)

            let to_z3 (ctx : Z3.context) (poly : polynomial) = Z3.Arithmetic.mk_add ctx  (List.map (ScaledMonomials.to_z3 ctx) poly)

            let get_coeff (mon : Monomials.monomial) (poly : polynomial) = 
                let mon_reduced_poly =(List.filter (fun scaled-> Monomials.equals (ScaledMonomials.get_monom scaled) mon) poly ) in
                   let coeff_list = List.map (ScaledMonomials.get_coeff) mon_reduced_poly in
                       List.fold_left (Big_int.add_big_int) Big_int.zero_big_int coeff_list
            
     end;;
