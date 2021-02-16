open Batteries
open Polynomials
open OurNum

exception Invalid_input of String.t
let list_string vec = 
    vec
    |> List.map OurInt.to_string
    |> String.concat ", "

let poly_vector_to_string vec =
    vec
    |> List.map Polynomial.to_string
    |> String.concat ", "

let make_string str = "\"" ^ str ^ "\""

module ProbUpdate =
    struct
        type t = {
            probability: OurNum.t;
            update: Polynomial.t list;
        }
        let from prob up = 
        {
            probability = prob;
            update = up;
        }

        let probability u =
            u.probability
        
        let update u =
            u.update

        let to_string pu =
            OurNum.to_string pu.probability ^ ":(" ^ poly_vector_to_string pu.update ^ ")"
    end

module ExactProgram =
    struct
        type t = {
            variables: Var.t list;
            guardpoly: Polynomial.t;
            guardvalue: OurInt.t;
            updates: ProbUpdate.t list;
            directtermination: ProbUpdate.t option;
            precision: OurInt.t option;
            initial: (OurInt.t list) option;
        }
        let from vars guardpoly guardval up dirterm prec initial =
        {
            variables = vars;
            guardpoly = guardpoly;
            guardvalue = guardval;
            updates = up;
            directtermination = dirterm;
            precision = prec;
            initial = initial;
        }

        let variables ep =
            ep.variables

        let guardpoly ep =
            ep.guardpoly

        let guardvalue ep =
            ep.guardvalue
        
        let updates ep =
            ep.updates
        
        let directtermination ep =
            ep.directtermination
        
        let precision ep =
            ep.precision

        let initial ep =
            ep.initial
        
        let updates_to_string ups =
            ups
            |> List.map ProbUpdate.to_string
            |> String.concat " :+: "
      
        let to_string ep =
            let guard_vec_str = "(GUARDPOLY " ^ Polynomial.to_string ep.guardpoly ^ ")\n" in
            let guard_val_str = "(GUARDVAL " ^ OurInt.to_string ep.guardvalue ^ ")\n" in
            let update_str = "(UPDATES " ^ updates_to_string ep.updates ^ ")\n" in
            let dir_term_str = Option.map (fun dterm -> "(DIRECTTERMINATION " ^ ProbUpdate.to_string dterm ^ ")\n") (directtermination ep) in
            let prec_str = Option.map (fun prec -> "(PRECISION " ^ OurInt.to_string prec ^ ")\n") (precision ep) in
            let init_str = Option.map (fun init -> "(INITIAL " ^ list_string init ^ ")\n") (initial ep ) in
            guard_vec_str ^ guard_val_str ^ update_str ^ (dir_term_str |? "") ^ (prec_str |? "") ^ (init_str |? "")

        let report_error str bool =
            if Bool.neg bool then
                raise (Invalid_input str);
            bool
        
        (** Check if the program is valid. *)
        let is_valid ep =
            (* Check whether only valid variables are used. *)
            let valid_variables ep =
                let valid_vars poly =
                    VarSet.subset (Polynomial.vars poly) (VarSet.of_list (variables ep))
                in
                let valid_guard =
                    ep
                    |> guardpoly
                    |> valid_vars
                    |> report_error "Guard uses invalid variables."
                in
                let valid_updates =
                    ep
                    |> updates
                    |> List.map ProbUpdate.update
                    |> List.map (List.map valid_vars)
                    |> List.map (List.for_all identity)
                    |> List.for_all identity
                    |> report_error "An updates uses invalid variables."
                in
                valid_guard && valid_updates
            in
            (* Check if all probabilities are greater than zero. *)
            let probabilities_greater_zero ep = 
                let dir_term_prob_greater_zero =
                    ep
                    |> directtermination
                    |> Option.map ProbUpdate.probability
                    |> Option.map (Num.(<=/) OurNum.zero) |? true
                in
                let update_prob_greater_zero =
                    ep
                    |> updates
                    |> List.map ProbUpdate.probability
                    |> List.for_all (Num.(<=/) OurNum.zero)
                in
                dir_term_prob_greater_zero && update_prob_greater_zero
                |> report_error "Not all Probabilities are greater than zero."
            in
            (* Check if all probabilities add up to one. *)
            let probabilities_equal_one ep =
                OurNum.(+) 
                ((ep |> directtermination |> Option.map ProbUpdate.probability) |? OurNum.zero)
                (ep |> updates |> List.map ProbUpdate.probability |> List.fold_left OurNum.(+) OurNum.zero)
                |> OurNum.equal OurNum.one
                |> report_error "Probabilities do not add up to one."
            in
            (* Check if all update polynomial vectors have the same length. *)
            let same_vec_len ep =
                let update_len =
                    ep |> variables |> List.length
                in
                let same_update_len =
                    ep |> updates |> List.map ProbUpdate.update |> List.map List.length
                    |> List.for_all (Int.equal update_len)
                    |> report_error "Not all update vectors have the same length."
                in
                let same_term_len = 
                    ep |> directtermination 
                    |> Option.map (fun dterm -> dterm |> ProbUpdate.update |> List.length)
                    |> Option.map_default (Int.equal update_len) true
                    |> report_error "Termination vector has the wrong length."
                in
                same_update_len && same_term_len
            in
            probabilities_equal_one ep 
            && probabilities_greater_zero ep 
            && same_vec_len ep 
            && valid_variables ep

        (** Check if the program can be transformed into a CP program. *)
        let is_cp_program ep =
            (* Compates two lists. Only returns true if a implies b. *)
            let rec comp_bool_lists l1 l2 = match l1, l2 with
                | [], []        -> true
                | [],  _ 
                | _ , []         -> false
                | true::xs, true::ys  -> true && comp_bool_lists xs ys
                | true::xs, false::ys -> false && comp_bool_lists xs ys
                | false::xs, _::ys -> true && comp_bool_lists xs ys
            in
            (* Get a vector booleans indicating whether which variables affect the runtime. *)
            let dep_vars =
                ep
                |> variables
                |> List.map (fun v -> ep |> guardpoly |> Polynomial.vars |> VarSet.mem v)
            in
            (* Get a vector of booleans indicating which entries of a update vector are constant. *)
            let const_entries update_vec =
                update_vec
                |> List.map Polynomial.is_const
            in
            (* Check if all updates affecting the runtime are constant. *)
            let const_updates =
                ep
                |> updates
                |> List.map ProbUpdate.update
                |> List.map const_entries
                |> List.map (comp_bool_lists dep_vars)
                |> List.mem false
                |> Bool.neg
                |> report_error "Not all updates that affect the runtime are constant."
            in
            (* Check if all direct termination updates affecting the runtime are constant. *)
            let const_dirterm =
                ep
                |> directtermination
                |> Option.map ProbUpdate.update
                |> Option.map const_entries
                |> Option.map (comp_bool_lists dep_vars) |? true
                |> report_error "Not all direct termination updates that affect the runtime are constant."
            in
            (* Check if the guard is linear. *)
            let linear_guard =
                ep
                |> guardpoly
                |> Polynomial.is_linear
                |> report_error "Guard is not linear."
            in
            const_updates && const_dirterm && linear_guard

        let to_python ep =
            let dep_vars =
                ep
                |> variables
                |> List.map (fun v -> ep |> guardpoly |> Polynomial.vars |> VarSet.mem v)
            in
            let make_update_constant probupdate =
                let rec _make_update_constant l1 l2 = match l1, l2 with
                    | [], []
                    | [],  _ 
                    | _ , []            -> []
                    | false::xs, y::ys   -> Polynomial.zero::(_make_update_constant xs ys)
                    | true::xs, y::ys  -> y::(_make_update_constant xs ys)
                in
                probupdate
                |> ProbUpdate.update
                |> _make_update_constant dep_vars
                |> ProbUpdate.from (ProbUpdate.probability probupdate)
            in
            let vector_from_poly poly =
                ep
                |> variables
                |> List.map (fun v -> Polynomial.coeff_of_var v poly)
            in
            let constant_part_of_guard =
                ep
                |> guardpoly
                |> Polynomial.get_constant
                |> OurInt.neg
            in
            let positional = 
                [
                    "(" ^ (ep |> guardpoly |> vector_from_poly |> List.map OurInt.to_string |> String.concat ", ") ^ ")";
                    ep |> guardvalue |> OurInt.(+) constant_part_of_guard |> OurInt.to_string;
                    ep |> updates |> List.map make_update_constant |> updates_to_string
                ]
                |> List.map make_string
            in
            let optional =
                [
                    ep |> directtermination |> Option.map make_update_constant |> Option.map ProbUpdate.to_string |> Option.map make_string |> Option.map (fun str -> "--dirterm " ^ str);
                    ep |> precision |> Option.map OurInt.to_string |> Option.map make_string |> Option.map (fun str -> "--prec " ^ str);
                    ep |> initial |> Option.map list_string |> Option.map (fun str -> "--init \"(" ^ str ^ ")\"")
                ]
                |> List.filter Option.is_some
                |> List.map Option.get
            in
            positional @ optional
            |> String.concat " "
        
  end