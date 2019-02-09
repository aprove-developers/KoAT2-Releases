%token	<string>	ID
%token	<int>		UINT
%token  <string>	UFLOAT
%token				PLUS MINUS TIMES POW
%token				EQUAL UNEQUAL GREATERTHAN GREATEREQUAL LESSTHAN LESSEQUAL
%token				LPAR RPAR
%token				LBRACE RBRACE
%token				EOF
%token          	OR
%token              AND
%token 				ARROW WITH PROBDIV LBRACK RBRACK
%token				GOAL STARTTERM FUNCTIONSYMBOLS RULES VAR 
%token              COMMA COLON
%token              MIN MAX INFINITY ABS
%token              UNIFORM
%token				GUARDVEC GUARDVAL UPDATES PRECISION DIRECTTERMINATION
%token <string>		FRACTION

%left				PLUS MINUS
%left				TIMES
%left				POW
                  
			
%start <Program.t> onlyProgram

%start <Program.t> onlyProgram_simple

%start <Formulas.Formula.t> onlyFormula

%start <Constraints.Constraint.t> onlyConstraints

%start <Atoms.Atom.t> onlyAtom

%start <Polynomials.Polynomial.t> onlyPolynomial

%start <BoundsInst.Bound.t> onlyBound

%start <(Program.t * string)> programAndGoal

%start <string> onlyGoal

%start <ExactProgramTypes.ExactProgram.t> exactProgram

%type <Program.t> program

%type <Formulas.Formula.t> formula

%type <Polynomials.Polynomial.t> polynomial

%type <(string * TransitionLabel.UpdateElement.t list)> transition_target

%type <ProbDistribution.t> dist

%type <TransitionLabel.UpdateElement.t> update_element

%type <Var.t list> variables

%type <string> goal

%{
  open BatTuple
  module Constr = Constraints.Constraint
  open Atoms
  open BoundsInst
  module Poly = Polynomials.Polynomial
  open Formulas
  open ProgramTypes
  open OurNum
  open ExactProgramTypes
%}

%%

onlyGoal :
        |       g = goal;
                { g };

onlyProgram :
        |       p = program; EOF
                  { p } ;

program :
        |       goal
                start = start
                variables = variables
                transitions = transitions
                  { Program.from (transitions variables) start } ;

programAndGoal :
	|	g = goal
		start = start
		variables = variables
                transitions = transitions ;EOF
                  { (Program.from (transitions variables) start, g) } ;

onlyProgram_simple :
        |       graph = program_simple; EOF
                  { graph } ;

program_simple :
	|       transitions = separated_nonempty_list(COMMA, transition_simple)
                  { ParserUtil.mk_program_simple (List.flatten transitions) } ;

transition_simple :
	|	start = ID; cost = cost ; rhs = non_prob_transition_rhs; formula = withConstraints
	          { ParserUtil.mk_transition_simple start cost rhs formula } ;
        |	start = ID; cost = cost ; rhs = prob_transition_rhs; formula = withConstraints
	          { ParserUtil.mk_transition_simple_prob start cost rhs formula } ;

goal :		
	|	LPAR GOAL goal = ID RPAR
                  { goal } ;

start :
	|	LPAR STARTTERM LPAR FUNCTIONSYMBOLS start = ID RPAR RPAR
		  { Location.of_string start } ;

transitions :
	|	LPAR RULES transition = nonempty_list(transition) RPAR
		  { fun vars -> List.map (fun t -> t vars) transition |> List.flatten } ;

variables :   
	|	LPAR VAR vars = list(ID) RPAR
		  { List.map Var.of_string vars } ;
		  
transition :
	|	lhs = transition_lhs; cost = cost ; rhs = non_prob_transition_rhs; formula = withConstraints
	          { ParserUtil.mk_transition lhs cost rhs formula } ;
        |       lhs = transition_lhs; cost = cost ; rhs = prob_transition_rhs; formula = withConstraints
	          { ParserUtil.mk_transition_prob lhs cost rhs formula } ;
		  
cost : 
        |       MINUS LBRACE ub = polynomial COMMA lb = polynomial RBRACE GREATERTHAN
                  { ub };
        |       MINUS ub = polynomial GREATERTHAN
                  { ub };
        |       MINUS LBRACE ub = polynomial RBRACE GREATERTHAN
                  { ub };
        |       ARROW
                  { Poly.one };
transition_lhs :
	|	start = ID; patterns = delimited(LPAR, separated_list(COMMA, ID), RPAR)
	          { (start, patterns) } ;
prob_transition_rhs :
        |       rhs = separated_nonempty_list(PROBDIV, non_prob_transition_rhs_with_prob)
                  { rhs };
                  
non_prob_transition_rhs_with_prob :
        |       prob = withProbabilities; old_rhs = non_prob_transition_rhs
                  { (prob, Batteries.Tuple2.first old_rhs, Batteries.Tuple2.second old_rhs) };
	          
non_prob_transition_rhs :
	|       com_kind = ID; LPAR targets = separated_nonempty_list(COMMA, transition_target) RPAR
                  { (com_kind, targets)} ;
        |       target = transition_target
                  { ("Com_1", [target]) } ;
transition_target :
	|       target = ID; LPAR assignments = separated_list(COMMA, update_element) RPAR
	          { (target, assignments) } ;

withConstraints :
	|	{ Formula.mk_true }
	|       LBRACK constr = separated_nonempty_list(AND, formula_atom) RBRACK { Formula.all constr } 
	|       WITH constr = separated_nonempty_list(AND, formula_atom) { Formula.all constr } ;
	
withProbabilities :
        |        prob = UFLOAT COLON
                { ParserUtil.ourfloat_of_decimal_string prob};
	

onlyFormula :
        |       f = formula EOF { f } ;

formula :
        |       disj = separated_nonempty_list(OR, formula_constraint)
                  { Formula.any disj } ;

onlyConstraints :
        |       constr = separated_list(AND, constraint_atom) EOF
                  { Constr.all constr } ;
        
formula_constraint :
        |       constr = separated_list(AND, formula_atom)
                  { Formula.all constr } ;

onlyAtom :
        |   	p1 = polynomial; comp = atom_comparator; p2 = polynomial; EOF
                  { comp p1 p2 } ;

constraint_atom :
        |   	p1 = polynomial; comp = constraint_comparator; p2 = polynomial
                  { comp p1 p2 } ;

formula_atom :
        |   	p1 = polynomial; comp = formula_comparator; p2 = polynomial
                  { comp p1 p2 } ;

%inline atom_comparator :
  	| 	GREATERTHAN { Atom.mk_gt }
  	| 	GREATEREQUAL { Atom.mk_ge }
  	| 	LESSTHAN { Atom.mk_lt }
  	| 	LESSEQUAL { Atom.mk_le } ;

%inline constraint_comparator :
  	| 	EQUAL { Constr.mk_eq }
  	| 	GREATERTHAN { Constr.mk_gt }
  	| 	GREATEREQUAL { Constr.mk_ge }
  	| 	LESSTHAN { Constr.mk_lt }
  	| 	LESSEQUAL { Constr.mk_le } ;

%inline formula_comparator :
  	| 	EQUAL { Formula.mk_eq }
  	| 	UNEQUAL { Formula.mk_uneq }
  	| 	GREATERTHAN { Formula.mk_gt }
  	| 	GREATEREQUAL { Formula.mk_ge }
  	| 	LESSTHAN { Formula.mk_lt }
  	| 	LESSEQUAL { Formula.mk_le } ;

              
onlyPolynomial :
        |       poly = polynomial EOF { poly } ;

variable :
	|	v = ID
                  { Poly.var v } ;

polynomial :
	|       v = variable
                  { v }
	| 	c = UINT
                  { Poly.value c }
	|	LPAR; ex = polynomial; RPAR
                  { ex }
	|       MINUS; ex = polynomial
	          { Poly.neg ex }
	|       p1 = polynomial; op = bioperator; p2 = polynomial
	          { op p1 p2 }
	|       v = variable; POW; c = UINT
	          { Poly.pow v c } ;

dist:
        |       UNIFORM; LPAR; p1 = polynomial; RPAR; LPAR; p2 = polynomial; RPAR
                  { ProbDistribution.Uniform (p1,p2) };

update_element: 
        |       p = polynomial
                  { TransitionLabel.UpdateElement.Poly p }
        |       d = dist
                  { TransitionLabel.UpdateElement.Dist d }

onlyBound :
        |       b = bound EOF { b } ;

bound :
	|	INFINITY
		{ Bound.infinity }
	|	LPAR; b = bound; RPAR
                { b }
	|	MAX LBRACE max = separated_nonempty_list(COMMA, bound) RBRACE
		{ Bound.maximum (BatList.enum max) }
	|	MIN LBRACE min = separated_nonempty_list(COMMA, bound) RBRACE
		{ Bound.minimum (BatList.enum min) }
	|	MINUS b = bound
		{ Bound.neg b }
	|	c = UINT b = option(preceded(POW, bound))
		{ Bound.exp (OurInt.of_int c) BatOption.(b |? Bound.one) }
	|       v = ID
	        { Bound.of_var_string v }
	|       ABS b = bound ABS
	        { Bound.abs b }
	|       b = bound POW c = UINT
	        { Bound.pow b c }
	|	b1 = bound; op = bound_bioperator; b2 = bound
		{ op b1 b2 } ;

%inline bound_bioperator :
	|	PLUS { Bound.add }
	|	TIMES { Bound.mul }
	|       MINUS { Bound.sub } ;

%inline bioperator :
	|	PLUS { Poly.add }
	|	TIMES { Poly.mul }
	|       MINUS { Poly.sub } ;


exactProgram :
	|	g = goal;
		guard_vec = guard_vector
		guard_val = guard_value
		updates = exact_updates
		d_term = ioption(direct_termination)
		precision = ioption(precision); EOF
		{ ExactProgram.from guard_vec guard_val updates d_term precision } ;

guard_vector :
	|	LPAR GUARDVEC guard_vec = vector RPAR
		{ guard_vec } ;

guard_value :
	|	LPAR GUARDVAL guard_val = int_val RPAR
		{ guard_val } ;

exact_updates :
	|	LPAR UPDATES updates = separated_nonempty_list(PROBDIV, prob_update) RPAR
		{ updates } ;

direct_termination :
	|	LPAR DIRECTTERMINATION; update = prob_update RPAR
		{ update } ;

precision :
	|	LPAR PRECISION; prec = UINT RPAR
		{ OurInt.of_int prec } ;

prob_update :
	|	prob = UINT COLON update = vector
		{ ProbUpdate.from (OurNum.of_int prob) update };
	|	prob = UFLOAT COLON update = vector
		{ ProbUpdate.from (OurNum.of_float_string prob) update };
	|	prob = FRACTION COLON update = vector
		{ ProbUpdate.from (OurNum.of_string prob) update };

vector :
	|	LPAR values = separated_nonempty_list(COMMA, int_val) RPAR
		{ values } ;
int_val :
	|	value = UINT
		{ OurInt.of_int value } ;
	|	MINUS value = UINT
		{ OurInt.of_int (-value) } ;

