(*Transitions of Integer Transition Systems*)
type t =   
        {
            name : string;
            
            tail : Locations.t;
            
            head : Locations.t;
            
            (*update: anytype
            
            guard : anytype*)
            
            (*cost : Polynomial in the variables;*)
            
        }
        
let equal (trans1 : t) (trans2 : t) =
    let eq_loc = Locations.equal in
        (eq_loc trans1.tail trans2.tail) && (eq_loc trans1.head trans2.head) && (trans1.name == trans2.name)
        
let compare (trans1 : t) (trans2 : t) = 
    if (equal trans1 trans2) then 0
    else if (trans1.name < trans2.name) then (-1)
    else 1
    
    let default =   
        {   
            name = "default" ;
            tail = Locations.default ;
            head = Locations.default ;
            cost = Big_int.zero_big_int
        }