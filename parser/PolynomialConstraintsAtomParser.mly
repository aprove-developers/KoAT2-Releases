%parameter<Var : PolyTypes.ID>

%token	<string>	ID
%token	<int>		UINT
%token			PLUS MINUS TIMES POW
%token			EQUAL 
%token			NEQ 
%token			GREATERTHAN 
%token			GREATEREQUAL 
%token			LESSTHAN 
%token			LESSEQUAL
%token			LPAR RPAR 
%token			EOF
			
%left			PLUS MINUS
%left			TIMES
%left			POW
%left                   EQUAL NEQ
%left                   GREATERTHAN GREATEREQUAL
%left                   LESSTHAN LESSEQUAL
			
%start <ConstraintTypes.PolynomialConstraintsAtomAST(Var).t> polynomialConstraintAtom

%type <ConstraintTypes.PolynomialConstraintsAtomAST(Var).polynomial> expression

%type <ConstraintTypes.PolynomialConstraintsAtomAST(Var).t> atom

%{
  module P = ConstraintTypes.PolynomialConstraintsAtomAST(Var)
%}
			
%%

polynomialConstraintAtom :
	|	at = atom EOF { at };

atom :
        |   p1 = expression; EQUAL ; p2 = expression
            {P.Equal(p1, p2)}
        |   p1 = expression; NEQ ; p2 = expression
            {P.Neq(p1, p2)}
        |   p1 = expression; GREATERTHAN ; p2 = expression
            {P.GreaterThan(p1, p2)}
        |   p1 = expression; GREATEREQUAL ; p2 = expression
            {P.GreaterEqual(p1, p2)}
        |   p1 = expression; LESSTHAN ; p2 = expression
            {P.LessThan(p1, p2)}
        |   p1 = expression; LESSEQUAL ; p2 = expression
            {P.LessEqual(p1, p2)}

variable :
	|	v = ID
                  { Var.of_string v }

expression :
	|       v = variable
                  { P.Variable v }
	| 	c = UINT
                  { P.Constant c }
	|	LPAR; ex = expression; RPAR
                  { ex }
	|       MINUS; ex = expression
	          { P.Neg ex }
	|       ex1 = expression; PLUS; ex2 = expression
	          { P.Plus (ex1, ex2) }
	|       ex1 = expression; TIMES; ex2 = expression
	          { P.Times (ex1, ex2) }
	|       ex1 = expression; MINUS; ex2 = expression
	          { P.Plus (ex1, P.Neg ex2) }
	|       v = variable; POW; c = UINT
	          { P.Pow (v, c) } ;
