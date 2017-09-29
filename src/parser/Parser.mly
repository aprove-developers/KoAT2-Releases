%token	<string>	ID
%token	<int>		UINT
%token			PLUS MINUS TIMES POW
%token			EQUAL GREATERTHAN GREATEREQUAL LESSTHAN LESSEQUAL
%token			LPAR RPAR
%token                  LBRACE RBRACE
%token			EOF
%token                  AND OR
%token 			ARROW WITH
%token			GOAL STARTTERM FUNCTIONSYMBOLS RULES VAR 
%token                  COMMA
%token                  MIN MAX NEG SUM PRODUCT INFINITY EXP

%left			PLUS MINUS
%left			TIMES
                  
			
%start <Program.t> onlyTransitiongraph

%start <Formula.Make(Polynomials.Make(PolyTypes.OurInt)).t> onlyFormula

%start <Constraints.Make(Polynomials.Make(PolyTypes.OurInt)).t> onlyConstraints

%start <Atoms.Make(Polynomials.Make(PolyTypes.OurInt)).t> onlyAtom

%start <Polynomials.Make(PolyTypes.OurInt).t> onlyPolynomial

%start <Program.TransitionLabel.Bound.t> onlyBound

%type <Program.t> transitiongraph

%type <Formula.Make(Polynomials.Make(PolyTypes.OurInt)).t> formula

%type <Constraints.Make(Polynomials.Make(PolyTypes.OurInt)).t> constraints

%type <Constraints.Make(Polynomials.Make(PolyTypes.OurInt)).t> atom

%type <Polynomials.Make(PolyTypes.OurInt).t> polynomial

%type <Var.t list> variables

%type <vars:Var.t list -> Program.TransitionLabel.t> transition

%type <(vars:Var.t list -> Program.TransitionLabel.t) list> transitions

%{
  open BatTuple
  module Constr = Constraints.Make(Polynomials.Make(PolyTypes.OurInt))
  module Atom = Atoms.Make(Polynomials.Make(PolyTypes.OurInt))
  module Poly = Polynomials.Make(PolyTypes.OurInt)
  module Bound = MinMaxPolynomial.Make(Polynomials.Make(PolyTypes.OurInt))
  module Formula_ = Formula.Make(Polynomials.Make(PolyTypes.OurInt))
%}

%%

onlyTransitiongraph :
        |       graph = transitiongraph; EOF
                  { graph } ;

transitiongraph :
        |       goal
                start = start
                vars = variables
                trans = transitions
                  { Program.from vars (List.map (fun t -> t ~vars) trans) start } ;

goal :		
	|	LPAR GOAL goal = ID RPAR
                  { goal } ;

start :
	|	LPAR STARTTERM LPAR FUNCTIONSYMBOLS start = ID RPAR RPAR
		  { Program.Location.of_string start } ;

transitions :
	|	LPAR RULES l = nonempty_list(transition) RPAR
		  { l } ;

variables :   
	|	LPAR VAR vars = list(ID) RPAR
		  { List.map Var.of_string vars } ;

transition :
	|	lhs = transition_lhs; ARROW; rhs = transition_rhs; constr = withConstraints
	          { Program.TransitionLabel.mk ~name:(Tuple2.first rhs)
                                     ~start:(Tuple2.first lhs)
                                     ~targets:(Tuple2.second rhs)
                                     ~patterns:(List.map Var.of_string (Tuple2.second lhs))
                                     ~guard:constr } ;

transition_lhs :
	|	start = ID; patterns = delimited(LPAR, separated_list(COMMA, ID), RPAR)
	          { (start, patterns) } ;

transition_rhs :
	|	name = ID; LPAR targets = separated_nonempty_list(COMMA, transition_target) RPAR
 	          { (name, targets) } ;

transition_target :
	|       target = ID; LPAR assignments = separated_list(COMMA, polynomial) RPAR
	          { (target, assignments) } ;

withConstraints :
	|	{ Constr.mk [] }
	|       WITH constr = separated_nonempty_list(AND, atom) { Constr.all constr } ;

onlyFormula :
        |       f = formula EOF { f } ;

formula :
        |       disj = separated_list(OR, constraints)
                  { Formula_.disj disj } ;

onlyConstraints :
        |       constr = constraints EOF { constr } ;
        
constraints :
        |       constr = separated_list(AND, atom)
                  { Constr.all constr } ;

onlyAtom :
        |   	p1 = polynomial; comp = atomComparator; p2 = polynomial; EOF
                  { comp p1 p2 } ;

%inline atomComparator :
  	| 	GREATERTHAN { Atom.mk_gt }
  	| 	GREATEREQUAL { Atom.mk_ge }
  	| 	LESSTHAN { Atom.mk_lt }
  	| 	LESSEQUAL { Atom.mk_le } ;

atom :
        |   	p1 = polynomial; comp = comparator; p2 = polynomial
                  { comp p1 p2 } ;

%inline comparator :
  	| 	EQUAL { Constr.mk_eq }
  	| 	GREATERTHAN { Constr.mk_gt }
  	| 	GREATEREQUAL { Constr.mk_ge }
  	| 	LESSTHAN { Constr.mk_lt }
  	| 	LESSEQUAL { Constr.mk_le } ;

              
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

onlyBound :
        |       b = bound EOF { b } ;

bound :
	|	p = polynomial
		{ Bound.of_poly p }
	|	MAX LBRACE max = separated_list(COMMA, bound) RBRACE
		{ Bound.maximum max }
	|	MIN LBRACE min = separated_list(COMMA, bound) RBRACE
		{ Bound.minimum min }
	|	NEG b = bound
		{ Bound.neg b }
	|	v = UINT; EXP; b = bound
		{ Bound.exp (Bound.Polynomial_.Value.of_int v) b }
	|	SUM LBRACE sum = separated_list(COMMA, bound) RBRACE
		{ Bound.sum sum }
	|	PRODUCT LBRACE product = separated_list(COMMA, bound) RBRACE
		{ Bound.product product }
	|	INFINITY
		{ Bound.infinity }

%inline bioperator :
	|	PLUS { Poly.add }
	|	TIMES { Poly.mul }
	|       MINUS { fun ex1 ex2 -> Poly.add ex1 (Poly.neg ex2) } ;
