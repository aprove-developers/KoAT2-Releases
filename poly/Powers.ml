open Mapping

type pow = 
    {
        var : Variables.variable; 
        n : int
    }

type value = Big_int.big_int
  
let to_string ( power : pow ) =
    if power.n<=0 then "1"
    else if power.n == 1 then (Variables.to_string power.var) 
        else String.concat "^" [(Variables.to_string power.var); (string_of_int power.n)]

let to_z3 (ctx : Z3. context)  ( power : pow ) =
    Z3.Arithmetic.mk_power ctx ( Variables.to_z3 ctx power.var ) (Z3.Arithmetic.Integer.mk_numeral_i ctx power.n)
    
let mk_pow_from_var (base : Variables.variable) (exponent : int) = 
    {
        var = base;
        n = exponent
    }

let mk_var (name : string) =
    {   var = Variables.mk_var name;
        n = 1
    }

let get_variable (power : pow) =
    power.var

let get_degree (power : pow) =
    power.n

let equal (power1 : pow) (power2 : pow) =
    (Variables.equal power1.var power2.var) && ( power1.n == power2.n)


let rename_power (varmapping : string VarMap.t) (power : pow) =
    {   var = Variables.get_new_var_name varmapping power.var;
        n = power.n
    }
    
let eval (varmapping : value VarMap.t) (power : pow) =
    if power.n < 0 then Big_int.zero_big_int
    else Big_int.power_big_int_positive_int (Variables.eval varmapping power.var) power.n
