%token <string> ID
%token <string> UINT
%token <string> UFLOAT
%token          PLUS MINUS TIMES POW
%token          EQUAL UNEQUAL GREATERTHAN GREATEREQUAL LESSTHAN LESSEQUAL
%token          LPAR RPAR
%token          LBRACE RBRACE
%token          EOF
%token          OR
%token          AND
%token          ARROW WITH PROBDIV LBRACK RBRACK
%token          GOAL STARTTERM FUNCTIONSYMBOLS RULES VAR
%token          COMMA COLON
%token          INFINITY
%token          EXPECTEDCOMPLEXITY COMPLEXITY EXACTRUNTIME EXPECTEDSIZE
%token          BERNOULLI BINOMIAL GEOMETRIC HYPERGEOMETRIC UNIFORM

%left           PLUS MINUS
%left           TIMES
%left           POW


%start <Program.t> onlyProgram

%start <ProbabilisticPrograms.ProbabilisticProgram.t> onlyProbabilisticProgram

%start <Program.t> onlyProgram_simple

%start <Formulas.Formula.t> onlyFormula

%start <Constraints.Constraint.t> onlyConstraints

%start <Atoms.Atom.t> onlyAtom

%start <Polynomials.Polynomial.t> onlyPolynomial

%start <ProbabilityDistribution.t> onlyProbabilityDistribution

%start <UpdateElement_.t> onlyUpdateElement

%start <ProbabilisticPrograms.GeneralTransitionSet.t> general_transitions

%start <BoundsInst.Bound.t> onlyBound

%start <Program.t * Goal.classical Goal.goal> programAndGoal

%start <ProbabilisticPrograms.ProbabilisticProgram.t * Goal.probabilistic Goal.goal> probabilisticProgramAndGoal

%type <Formulas.Formula.t> formula

%type <Polynomials.Polynomial.t> polynomial

%type <Var.t list> variables

%{
  open BatTuple
  open Constraints
  open Atoms
  open Polynomials
  open Formulas
  open ProgramModules
  open BoundsInst
%}

%%

onlyProgram:
  | p_and_g = programAndGoal
    { Tuple2.first p_and_g } ;

onlyProbabilisticProgram:
  | p_and_g = probabilisticProgramAndGoal
    { Tuple2.first p_and_g };

programAndGoal:
  | g = goal
    start = start
    variables = variables
    transitions = transitions; EOF
    { Program.from_com_transitions (transitions variables) start, g } ;

probabilisticProgramAndGoal:
  | g = probabilisticGoal
    start = start
    variables = variables;
    general_transitions = general_transitions; EOF
    { ParserUtil.mk_probabilistic_program start general_transitions, g }

onlyProgram_simple:
  | graph = program_simple; EOF
    { graph } ;

program_simple:
  | transitions = separated_nonempty_list(COMMA, transition_simple)
    { ParserUtil.mk_program_simple (List.flatten transitions) } ;

transition_simple:
  | start = ID; cost = cost ; rhs = transition_rhs; formula = withConstraints
    { ParserUtil.mk_transition_simple start cost rhs formula } ;

goal:
  | LPAR GOAL COMPLEXITY RPAR
    { Goal.Complexity }
  | LPAR GOAL EXACTRUNTIME RPAR
    { Goal.ExactRuntime }

probabilisticGoal:
  | LPAR GOAL EXPECTEDCOMPLEXITY RPAR
    { Goal.ExpectedComplexity }
  | LPAR GOAL EXPECTEDSIZE var=ID RPAR
    { Goal.ExpectedSize (Var.of_string var) }

start:
  | LPAR STARTTERM LPAR FUNCTIONSYMBOLS start = ID RPAR RPAR
    { Location.of_string start } ;

transitions:
  | LPAR RULES transition = list(transition) RPAR
    { fun vars -> List.map (fun t -> t vars) transition |> List.flatten } ;

general_transitions:
  | LPAR RULES general_transitions = list(general_transition) RPAR
    { ParserUtil.mk_general_transitions general_transitions } ;

variables:
  | LPAR VAR vars = list(ID) RPAR
    { List.map Var.of_string vars } ;

transition:
  | lhs = transition_lhs; cost = cost ; rhs = transition_rhs; formula = withConstraints
    { ParserUtil.mk_transition lhs cost rhs formula } ;

(* We construct the ProbabilisticTransitionLabels in a delayed fashion since we might have to copy them, e.g. the guard
       contains multiple constraints. The delayed construction ensures that every Label gets a unique id *)
general_transition:
  | lhs = transition_lhs; cost = cost; rhss = general_transition_rhss; constr = withConstraints
    { (lhs, cost,
       List.map
         (fun r -> ParserUtil.embed_probabilistic_transition_label lhs r) rhss, constr) } ;

cost:
  | MINUS LBRACE ub = polynomial COMMA lb = polynomial RBRACE GREATERTHAN
    { ub }
  | MINUS ub = polynomial GREATERTHAN
    { ub }
  | MINUS LBRACE ub = polynomial RBRACE GREATERTHAN
    { ub }
  | ARROW
    { Polynomial.one };

transition_lhs:
  | start = ID; patterns = delimited(LPAR, separated_list(COMMA, ID), RPAR)
    { (start, patterns) } ;

transition_rhs:
 | com_kind = ID; LPAR targets = separated_nonempty_list(COMMA, transition_target) RPAR
   { (com_kind, targets) }
 | target = transition_target
   { ("Com_1", [target]) } ;

transition_target:
  | target = ID; LPAR assignments = separated_list(COMMA, polynomial) RPAR
    { (target, assignments) } ;

(* TODO: Alternatively parse classical transition here *)
general_transition_rhss:
  | rhss = separated_nonempty_list(PROBDIV, prob_transition_rhs_with_prob)
    { rhss }
    (** If there is only one transition then we allow to omit the probability annotation *)
  | rhs = prob_transition_rhs_without_prob
    { [(OurFloat.one,rhs)] };

prob_transition_rhs_with_prob:
  | prob=our_float; COLON; rhs_without_prob=prob_transition_rhs_without_prob
    { (prob,rhs_without_prob) }

prob_transition_rhs_without_prob:
 | com_kind = ID; LPAR targets = separated_nonempty_list(COMMA, probabilistic_transition_target) RPAR
   { (com_kind, targets) }
 | target = probabilistic_transition_target
   { ("Com_1", [target]) } ;

probabilistic_transition_target:
  | target = ID; LPAR assignments = separated_list(COMMA, update_element) RPAR
    { (target,assignments) };

update_element:
  | v = update_value
    { v }
  | c = our_int
    { UpdateElement_.of_constant c }
  | LPAR; ex = update_element; RPAR
    { ex }
  | MINUS; ex = update_element
    { UpdateElement_.neg ex }
  | p1 = update_element; op = update_element_bioperator; p2 = update_element
    { op p1 p2 }
  | v = update_value; POW; c = UINT
    { UpdateElement_.pow v (int_of_string c) } ;

update_value:
  | d = onlyProbabilityDistribution
    { UpdateElement_.of_dist d }
  | v = ID
    { UpdateElement_.var v } ;

onlyProbabilityDistribution:
  | BERNOULLI; LPAR; p = our_float; RPAR
    { ProbabilityDistribution.Binomial (Polynomial.one,p) }
  | BINOMIAL; LPAR; n = polynomial; COMMA; p = our_float; RPAR
    { ProbabilityDistribution.Binomial (n,p) }
  | GEOMETRIC; LPAR; p = our_float; RPAR
    { ProbabilityDistribution.Geometric p }
  | HYPERGEOMETRIC; LPAR; bigN = our_int; COMMA; k = polynomial; COMMA; n = polynomial; RPAR
    { ProbabilityDistribution.Hypergeometric (bigN,k,n) }
  | UNIFORM; LPAR; p1 = polynomial; COMMA; p2 = polynomial; RPAR
    { ProbabilityDistribution.Uniform (p1,p2) }

withConstraints:
  | { Formula.mk_true }
  | WITH constr = separated_nonempty_list(AND, formula_atom)
    { Formula.all constr }
  | LBRACK constr = separated_nonempty_list(AND, formula_atom) RBRACK
    { Formula.all constr } ;

onlyFormula:
  | f = formula EOF
    { f } ;

formula:
  | disj = separated_nonempty_list(OR, formula_constraint)
    { Formula.any disj } ;

onlyConstraints:
  | constr = separated_list(AND, constraint_atom) EOF
    { Constraint.all constr } ;

formula_constraint:
        | constr = separated_list(AND, formula_atom)
            { Formula.all constr } ;

onlyAtom:
  | p1 = polynomial; comp = atom_comparator; p2 = polynomial; EOF
    { comp p1 p2 } ;

constraint_atom:
  | p1 = polynomial; comp = constraint_comparator; p2 = polynomial
    { comp p1 p2 } ;

formula_atom:
  | p1 = polynomial; comp = formula_comparator; p2 = polynomial
    { comp p1 p2 } ;

%inline atom_comparator:
  | GREATERTHAN { Atom.mk_gt }
  | GREATEREQUAL { Atom.mk_ge }
  | LESSTHAN { Atom.mk_lt }
  | LESSEQUAL { Atom.mk_le } ;

%inline constraint_comparator:
  | EQUAL { Constraint.mk_eq }
  | GREATERTHAN { Constraint.mk_gt }
  | GREATEREQUAL { Constraint.mk_ge }
  | LESSTHAN { Constraint.mk_lt }
  | LESSEQUAL { Constraint.mk_le } ;

%inline formula_comparator:
  | EQUAL { Formula.mk_eq }
  | UNEQUAL { Formula.mk_uneq }
  | GREATERTHAN { Formula.mk_gt }
  | GREATEREQUAL { Formula.mk_ge }
  | LESSTHAN { Formula.mk_lt }
  | LESSEQUAL { Formula.mk_le } ;


onlyPolynomial:
  | poly = polynomial EOF { poly } ;

onlyUpdateElement:
  | ue = update_element EOF { ue };

variable:
  | v = ID
    { Polynomial.var v } ;

our_float:
  | float_string = UFLOAT
    { ParserUtil.ourfloat_of_decimal_or_fraction_string float_string } ;

our_int:
  | int_string = UINT
    { OurInt.of_string int_string } ;

polynomial:
  | v = variable
    { v }
  | c = our_int
    { Polynomial.of_constant c }
  | LPAR; ex = polynomial; RPAR
    { ex }
  | MINUS; ex = polynomial
    { Polynomial.neg ex }
  | p1 = polynomial; op = bioperator; p2 = polynomial
    { op p1 p2 }
  | v = variable; POW; c = UINT
    { Polynomial.pow v (int_of_string c) } ;

onlyBound:
  | b = bound EOF { b } ;

bound:
  | INFINITY
    { Bound.infinity }
  | LPAR; b = bound; RPAR
    { b }
  | c = our_int b = option(preceded(POW, bound))
    { Bound.exp c BatOption.(b |? Bound.one) }
  | v = ID
    { Bound.of_var_string v }
  | b = bound POW c = UINT
    { Bound.pow b (int_of_string c) }
  | b1 = bound; op = bound_bioperator; b2 = bound
    { op b1 b2 } ;

%inline bound_bioperator:
  | PLUS { Bound.add }
  | TIMES { Bound.mul }

%inline update_element_bioperator:
  | PLUS  { UpdateElement_.add }
  | TIMES { UpdateElement_.mul }
  | MINUS { UpdateElement_.sub } ;

%inline bioperator:
  | PLUS { Polynomial.add }
  | TIMES { Polynomial.mul }
  | MINUS { Polynomial.sub } ;
