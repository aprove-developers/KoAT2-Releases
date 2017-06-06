module Variable =
    struct 
        type variable = Var of string
        
        let mk_var (name : string) =
            Var name

        let to_string ( var : variable ) =
            match var with
                |Var str -> str
        let varlist_to_string (vars : variable list) =
            (String.concat ", " (List.map to_string  vars))
           
    end;;

module Location =
    struct
        type location = Start of Variable.variable list| Loc of int* Variable.variable list
    
        let to_string ( l : location ) =
            match l with
                |Start xs-> (String.concat " " ["startLocation ";"Variables ="; Variable.varlist_to_string xs])
                |Loc (num, xs) -> (String.concat " " ["l";(string_of_int num); "Variables ="; Variable.varlist_to_string xs]) 
	
        let mk_loc (n : int) ( vars: Variable.variable list) =
            if n <= 0 then Start vars
            else Loc (n,  vars)
    end;;

module Transition =
    struct
    
    
    end;;
