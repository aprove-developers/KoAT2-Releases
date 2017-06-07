let rec remove_dup (inputList : 'a list) =
    match inputList with
        |[] -> []
        |h::tail -> h:: remove_dup (List.filter (fun x->x <> h) tail)
