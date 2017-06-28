open Mapping

type pow = Pow of Variables.variable * int

let to_string ( power : pow ) =
    match power with
        |Pow (var, n ) -> 
            if n<=0 then "1"
            else if n == 1 then (Variables.to_string var) 
                else String.concat "^" [(Variables.to_string var); (string_of_int n)]

let to_z3 (ctx : Z3. context)  ( power : pow ) =
    match power with
        |Pow (var, n ) -> Z3.Arithmetic.mk_power ctx ( Variables.to_z3 ctx var ) (Z3.Arithmetic.Integer.mk_numeral_i ctx n)
let mk_pow_from_var (var : Variables.variable) (n : int) = Pow (var,n)

let mk_var (name : string) (n : int) = Pow ((Variables.mk_var name), n)

let get_variable (power : pow) =
    match power with 
        |Pow (var,n) -> var

let get_degree (power : pow) =
    match power with 
        |Pow (var,n) -> n

let equal (power1 : pow) (power2 : pow) =
    match (power1, power2) with
        |(Pow (var1,n1), Pow (var2, n2)) -> (Variables.equal var1 var2) && ( n1 == n2)


let rename_power (varmapping : string VarMap.t) (power : pow) =
    match power with
        |(Pow (var, n)) ->  Pow ((Variables.get_new_var_name varmapping var) , n)


let instantiate_with_big_int (varmapping : Big_int.big_int VarMap.t) (power : pow) =
    match power with
        |(Pow (var, n)) -> 
            if n < 0 then Big_int.zero_big_int
            else Big_int.power_big_int_positive_int (Variables.instantiate_with_big_int varmapping var) n
