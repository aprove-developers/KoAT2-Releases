%token <string> ID
%token <string> UINT
%token          PLUS MINUS TIMES POW
%token          EQUAL UNEQUAL GREATERTHAN GREATEREQUAL LESSTHAN LESSEQUAL
%token          LPAR RPAR
%token          LBRACE RBRACE
%token          EOF
%token          OR
%token          AND
%token          ARROW WITH LBRACK RBRACK
%token          GOAL STARTTERM FUNCTIONSYMBOLS RULES VAR
%token          COMMA
%token          INFINITY
%token          EXPECTEDCOMPLEXITY COMPLEXITY EXACTRUNTIME EXPECTEDSIZE

%left           PLUS MINUS
%left           TIMES
%left           POW


%start <Program.t> onlyProgram

%start <Program.t> onlyProgram_simple

%start <Formulas.Formula.t> onlyFormula

%start <Constraints.Constraint.t> onlyConstraints

%start <Atoms.Atom.t> onlyAtom

%start <Polynomials.Polynomial.t> onlyPolynomial

%start <BoundsInst.Bound.t> onlyBound

%start <Program.t * Goal.classical Goal.goal> programAndGoal

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

programAndGoal:
  | g = goal
    start = start
    variables = variables
    transitions = transitions; EOF
    { Program.from_com_transitions (transitions variables) start, g } ;

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

start:
  | LPAR STARTTERM LPAR FUNCTIONSYMBOLS start = ID RPAR RPAR
    { Location.of_string start } ;

transitions:
  | LPAR RULES transition = nonempty_list(transition) RPAR
    { fun vars -> List.map (fun t -> t vars) transition |> List.flatten } ;

variables:
  | LPAR VAR vars = list(ID) RPAR
    { List.map Var.of_string vars } ;

transition:
  | lhs = transition_lhs; cost = cost ; rhs = transition_rhs; formula = withConstraints
    { ParserUtil.mk_transition lhs cost rhs formula } ;

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

variable:
  | v = ID
    { Polynomial.var v } ;

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

%inline bioperator:
  | PLUS { Polynomial.add }
  | TIMES { Polynomial.mul }
  | MINUS { Polynomial.sub } ;
