%parameter<P : PolyTypes.ParseablePolynomial>

%token	<string>	ID
%token	<int>		UINT
%token			PLUS MINUS TIMES POW
%token			LPAR RPAR
%token			EOF
			
%left			PLUS MINUS
%left			TIMES
%left			POW
			
%start <P.t> polynomial

%type <P.t> expression

%%

polynomial :
	|	ex = expression EOF { ex };

variable :
	|	v = ID
                  { P.from_var_string v }

expression :
	|       v = variable
                  { v }
	| 	c = UINT
                  { P.from_constant_int c }
	|	LPAR; ex = expression; RPAR
                  { ex }
	|       MINUS; ex = expression
	          { P.neg ex }
	|       ex1 = expression; PLUS; ex2 = expression
	          { P.add ex1 ex2 }
	|       ex1 = expression; TIMES; ex2 = expression
	          { P.mul ex1 ex2 }
	|       ex1 = expression; MINUS; ex2 = expression
	          { P.add ex1 (P.neg ex2) }
	|       v = variable; POW; c = UINT
	          { P.pow v c } ;
