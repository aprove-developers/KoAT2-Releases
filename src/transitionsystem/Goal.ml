type probabilistic_koat_goal = ExpectedComplexity
                             | ExpectedSize of Var.t

type classical
type probabilistic

type _ goal =
  | Complexity: classical goal
  | ExactRuntime: classical goal
  | ExpectedComplexity: probabilistic goal
  | ExpectedSize: Var.t -> probabilistic goal
  | Unknown: 'a goal


let supported_analyse_goals = ["COMPLEXITY"; "EXPECTEDCOMPLEXITY"; "EXACTRUNTIME"; "EXPECTEDSIZE"]

let to_string: type a. a goal -> string = function
  | Complexity ->
      "COMPLEXITY"
  | ExactRuntime ->
      "EXACTRUNTIME"
  | ExpectedComplexity ->
      "ExpectedComplexity"
  | ExpectedSize v ->
      "EXPECTEDSIZE " ^ Var.to_string v
  | Unknown ->
      "Unknown"
