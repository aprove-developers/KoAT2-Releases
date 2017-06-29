open Mapping

type variable = string

let mk_var (name : string) =
    name

let to_string ( var : variable ) = var

let varlist_to_string (vars : variable list) =
    (String.concat ", " (List.map to_string  vars))

let equal (var1 : variable)  (var2 : variable) = (var1 == var2)

let to_z3 (ctx : Z3.context) (var : variable) =
    Z3.Arithmetic.Integer.mk_const ctx (Z3.Symbol.mk_string ctx var)

let get_new_var_name (varmapping : string VarMap.t) (var : variable) =
    if VarMap.mem var varmapping then
        VarMap.find var varmapping
    else var

let instantiate_with_big_int (varmapping : Big_int.big_int VarMap.t) (var : variable) =
    if VarMap.mem var varmapping then
        VarMap.find var varmapping
    else Big_int.zero_big_int 
