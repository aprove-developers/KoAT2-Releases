open ID

type var = Variables.MakeVariableTerm(StringID).t
type valuation = Variables.MakeVariableTerm(StringID).valuation
type t = Powers.MakePower(StringID).t list
type value = Variables.MakeVariableTerm(StringID).value
val to_z3 : Z3.context -> t -> Z3.Expr.expr
val make : (var * int) list -> t
val lift : Powers.MakePower(StringID).t -> t
val vars : t -> var list
val degree : t -> int
val degree_variable : var -> t -> int
val delete_var : var -> t -> Powers.MakePower(StringID).t list
val simplify : t -> t
val to_string_simplified : t -> string
val to_string : t -> string
val equal_simplified : t -> t -> bool
val (==) : t -> t -> bool
val is_univariate_linear_monomial : t -> bool
val rename : Variables.MakeVariableTerm(StringID).rename_map -> t -> t
val mult : t -> t -> t
val eval : valuation -> t -> value
