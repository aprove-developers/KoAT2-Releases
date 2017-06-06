open Types
let _ =	
    let vars = [(Variable.mk_var "X");(Variable.mk_var "Y")] in
    let startLoc = (Location.mk_loc 0  vars) in
    let testLoc = (Location.mk_loc 7 vars) in
            Printf.printf "Location: %s \n" (Location.to_string startLoc);
     	    Printf.printf "Location: %s \n" (Location.to_string testLoc)


;;
