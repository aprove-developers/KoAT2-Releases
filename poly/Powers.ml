open Mapping

type t = 
    {
        var : Variables.t; 
        n : int
    }

type value = Big_int.big_int
  
let to_string power =
    if power.n<=0 then "1"
    else if power.n == 1 then (Variables.to_string power.var) 
        else String.concat "^" [(Variables.to_string power.var); (string_of_int power.n)]

let to_z3 ctx power =
    Z3.Arithmetic.mk_power ctx ( Variables.to_z3 ctx power.var ) (Z3.Arithmetic.Integer.mk_numeral_i ctx power.n)
    
let mk_pow_from_var base exponent = 
    {
        var = base;
        n = exponent
    }

let mk_var name =
    {
        var = Variables.mk_var name;
        n = 1
    }

let get_variable power = power.var

let get_degree power = power.n

let equal power1 power2 =
    (Variables.equal power1.var power2.var) && ( power1.n == power2.n)


let rename_power varmapping power =
    {
        var = Variables.get_new_var_name varmapping power.var;
        n = power.n
    }
    
let eval varmapping power =
    if power.n < 0 then Big_int.zero_big_int
    else Big_int.power_big_int_positive_int (Variables.eval varmapping power.var) power.n
