let rec remove_dup (inputList : 'a list) =
    match inputList with
        |[] -> []
        |h::tail -> h:: remove_dup (List.filter (fun x->x <> h) tail)

let rec max_of_int_list (inputList : int list) =
    match inputList with
        |[] -> min_int 
        | a :: tail -> let max_tail = (max_of_int_list tail) in 
            if (a > max_tail)  then a else max_tail
