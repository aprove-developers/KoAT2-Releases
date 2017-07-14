%token	<string>	ID
%token	<int>		UINT
%token			PLUS MINUS TIMES POW
%token			LPAR RPAR
%token			EOF
			
%left			PLUS MINUS
%left			TIMES
%left			POW
			
%start <PolynomialType.t> polynomial

%type <PolynomialType.t> expression

%{
  open PolynomialType
%}
			
%%

polynomial :
	|	ex = expression EOF { ex };

expression :
	|	v = ID
                  { Variable v }
	| 	c = UINT
                  { Constant c }
	|	LPAR; ex = expression; RPAR
                  { ex }
	|       MINUS; ex = expression
	          { Neg ex }
	|       ex1 = expression; PLUS; ex2 = expression
	          { Plus (ex1, ex2) }
	|       ex1 = expression; TIMES; ex2 = expression
	          { Times (ex1, ex2) }
	|       ex1 = expression; MINUS; ex2 = expression
	          { Plus (ex1, Neg ex2) }
	|       ex = expression; POW; c = UINT
	          { Pow (ex, c) } ;
