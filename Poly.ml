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
                    | Scaled (coeff, mon)-> Z3.Arithmetic.mk_mul ctx [(Z3.Arithmetic.Integer.mk_numeral_s ctx (Big_int.string_of_big_int coeff)) ; (Monomials.to_z3 ctx mon)]
    end;;

module Polynomials =
    struct
        (*A polynomial is a scaled sum of monomials, the coefficients are integers*)
        
        type polynomial = ScaledMonomials.scaledMon list 
 
            let to_string (poly : polynomial) = String.concat "+" (List.map ScaledMonomials.to_string poly)


            let to_z3 (ctx : Z3.context) (poly : polynomial) = Z3.Arithmetic.mk_add ctx  (List.map (ScaledMonomials.to_z3 ctx) poly)
            
     end;;
