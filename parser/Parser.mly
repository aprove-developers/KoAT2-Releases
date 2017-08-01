%parameter<G : Parseable.TransitionGraph>

%token	<string>	ID
%token	<int>		UINT
%token			PLUS MINUS TIMES POW
%token			EQUAL NEQ GREATERTHAN GREATEREQUAL LESSTHAN LESSEQUAL
%token			LPAR RPAR
%token			EOF EOL
%token                  AND
%token 			ARROW WITH
%token			RULES VAR
%token                  COMMA

%left                   EQUAL NEQ LESSTHAN LESSEQUAL GREATERTHAN GREATEREQUAL
%left			PLUS MINUS
%left			TIMES
%left			POW
                  
			
%start <G.t> onlyTransitiongraph

%start <G.Transition_.Constraint_.t> onlyConstraints

%start <G.Transition_.Constraint_.Atom_.t> onlyAtom

%start <G.Transition_.Constraint_.Atom_.Polynomial_.t> onlyPolynomial

%type <G.t> transitiongraph

%type <G.Transition_.Constraint_.t> constraints

%type <G.Transition_.Constraint_.Atom_.t> atom

%type <G.Transition_.Constraint_.Atom_.Polynomial_.t> polynomial

%type <G.Transition_.Constraint_.Atom_.Polynomial_.Var.t list> variables

%type <string * string * (G.Transition_.Constraint_.Atom_.Polynomial_.Var.t list -> G.Transition_.t)> transition

%type <(string * string * (G.Transition_.Constraint_.Atom_.Polynomial_.Var.t list -> G.Transition_.t)) list> transitions

%{
  open BatTuple
  module Constr = G.Transition_.Constraint_
  module Atom = G.Transition_.Constraint_.Atom_
  module Poly = G.Transition_.Constraint_.Atom_.Polynomial_
%}

%%

onlyTransitiongraph :
        |       graph = transitiongraph EOF
                  { graph }

transitiongraph :
	|       vars = variables; list(EOL); trans = transitions
		  { G.from vars (List.map (fun (start, target, t) -> (start, target, t vars)) trans) }

transitions :
	|	l = delimited(LPAR, preceded(RULES, separated_list(EOL, transition)), RPAR)
		  { l }

variables :   
	|	vars = delimited(LPAR, preceded(VAR, list(ID)), RPAR)
		  { List.map Poly.Var.of_string vars }

transition_lhs :
	|	start = ID; patterns = delimited(LPAR, separated_list(COMMA, ID), RPAR)
	          { (start, patterns) }

transition_rhs :
	|	name = ID; LPAR target = ID; assignments = delimited(LPAR, separated_list(COMMA, polynomial), RPAR) RPAR
	          { (target, name, assignments) }

transition :
	|	lhs = transition_lhs; ARROW; rhs = transition_rhs; constr = withConstraints
                  { (Tuple2.first lhs, Tuple3.first rhs, G.Transition_.mk (Tuple3.second rhs) (List.map Poly.Var.of_string (Tuple2.second lhs)) (Tuple3.third rhs) constr) }

withConstraints :
	|	{ Constr.mk [] }
	|       constr = preceded(WITH, separated_nonempty_list(AND, atom)) { Constr.mk constr }

onlyConstraints :
        |       constr = constraints EOF { constr }
        
constraints :
        |       constr = separated_list(AND, atom)
                  {Constr.mk constr}

              
onlyAtom :
	|	at = terminated(atom, EOF) { at };
	
atom :
        |   	p1 = polynomial; comp = comparator; p2 = polynomial
                  { comp p1 p2 }
                
%inline comparator :
  	| 	EQUAL { Atom.mk_eq }
  	| 	NEQ { Atom.mk_neq }
  	| 	GREATERTHAN { Atom.mk_gt }
  	| 	GREATEREQUAL { Atom.mk_ge }
  	| 	LESSTHAN { Atom.mk_lt }
  	| 	LESSEQUAL { Atom.mk_le }

              
onlyPolynomial :
        |       poly = polynomial EOF { poly }

variable :
	|	v = ID
                  { Poly.from_var_string v }

polynomial :
	|       v = variable
                  { v }
	| 	c = UINT
                  { Poly.from_constant_int c }
	|	LPAR; ex = polynomial; RPAR
                  { ex }
	|       MINUS; ex = polynomial
	          { Poly.neg ex }
	|       p1 = polynomial; op = bioperator; p2 = polynomial
	          { op p1 p2 }
	|       v = variable; POW; c = UINT
	          { Poly.pow v c } ;

%inline bioperator :
	|	PLUS { Poly.add }
	|	TIMES { Poly.mul }
	|       MINUS { fun ex1 ex2 -> Poly.add ex1 (Poly.neg ex2) }
