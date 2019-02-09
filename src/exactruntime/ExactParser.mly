%token  <string>	FLOAT
%token  <string>	FRACTION
%token  <string>	VAR
%token				PLUS MINUS TIMES POW
%token				LPAR RPAR LBRK RBRK
%token              COMMA
%token				EOF
%token				COS SIN

%left				PLUS
%left				TIMES
%left				POW

%start <ExactBound.t list> exactBoundList


%{
  open ExactBound
  open Polynomials
%}

%%

exactBoundList :
    |   LBRK; bound_list = separated_nonempty_list(COMMA, exactBound) RBRK; EOF
		{ bound_list } ;

exactBound :
	|	factors = separated_nonempty_list(TIMES, factor)
		{ ExactBound.list_prod factors } 

factor :
	|	b = signedConstBound
		{ b }
	|	b = var
		{ b }
	|	base = powBase; POW; exp = powExp
		{ ExactBound.pow base exp }

powBase :
	|	v = const
		{ v }
	|	LPAR; MINUS; v = const; RPAR
		{ OurNum.neg v }

powExp :
	|	b = var
		{ b }


signedConstBound :
	|	MINUS; b = constBound
		{ ExactBound.neg b }
	|	b = constBound
		{ b }

constBound :
	|	c = const
		{ ExactBound.const c } ;

const :
	|	v = FLOAT
		{ OurNum.of_float_string v } ;
	|	v = FRACTION
		{ OurNum.of_string v } ;

var :
	|   var = VAR
		{ ExactBound.var (Var.of_string var)} ;
