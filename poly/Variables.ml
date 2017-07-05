open Mapping

type t = string

type value = Big_int.big_int

let mk_var name = name

let to_string var = var

let varlist_to_string vars =
    String.concat ", " (List.map to_string vars)

let equal = (==)

let to_z3 ctx var =
    Z3.Arithmetic.Integer.mk_const ctx (Z3.Symbol.mk_string ctx var)

let get_new_var_name varmapping var =
    if VarMap.mem var varmapping then
        VarMap.find var varmapping
    else var

let eval varmapping var =
    if VarMap.mem var varmapping then
        VarMap.find var varmapping
    else Big_int.zero_big_int 

let rec equal_varlist (list1 : t list) (list2 : t list) =  
    match (list1, list2) with
        | ([],[]) -> true
        | (x1::tail1, x2::tail2) -> (equal x1 x2) && (equal_varlist tail1 tail2)
        | ([],_) -> false
        | (_,[]) -> false