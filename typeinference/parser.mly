%{
	open Ast
%}

%token BACKSLASH DOT LPAREN RPAREN IDENTIFIER EOF INTEGER PLUS LET IN EQUALS
%token TIMES PRINT INT ARROW COLON FIX REC IF THEN ELSE GREATER LESS BOOL MINUS

%type<Ast.expr> expression simple_expr apply_expr plus_expr times_expr
%type<string> IDENTIFIER
%type<int> INTEGER
%type<Ast.ltype> type
%type<bool> BOOL

%start expression
%%

expression:
	| BACKSLASH IDENTIFIER DOT expression
								{ Abstraction($2, new_typevar(), $4) }
	| BACKSLASH IDENTIFIER COLON type DOT expression
								{ Abstraction($2, $4, $6) }
	| LET IDENTIFIER EQUALS expression IN expression
								{ Application(Abstraction($2, new_typevar(), $6), $4) }
	| LET IDENTIFIER COLON type EQUALS expression IN expression
								{ Application(Abstraction($2, $4, $8), $6) }
	| LET REC IDENTIFIER EQUALS expression IN expression
								{ Application(Abstraction($3, new_typevar(), $7), Fix(Abstraction($3, new_typevar(), $5))) }
	| LET REC IDENTIFIER COLON type EQUALS expression IN expression
								{ Application(Abstraction($3, $5, $9), Fix(Abstraction($3, $5, $7))) }
	| IF expression THEN expression ELSE expression
								{ If($2, $4, $6) }
	| PRINT expression			{ Unop(Print, $2) }
	| FIX expression			{ Fix($2) }
	| equals_expr				{ $1 }

equals_expr:
	plus_expr EQUALS plus_expr	{ Boolbinop(Equals, $1, $3) }
	| plus_expr GREATER plus_expr
								{ Boolbinop(Greater, $1, $3) }
	| plus_expr LESS plus_expr	{ Boolbinop(Less, $1, $3) }
	| plus_expr					{ $1 }

plus_expr:
	times_expr PLUS plus_expr	{ Binop(Plus, $1, $3) }
	| times_expr MINUS plus_expr
								{ Binop(Minus, $1, $3) }
	| times_expr				{ $1 }

times_expr:
	apply_expr TIMES times_expr	{ Binop(Times, $1, $3) }
	| apply_expr				{ $1 }

apply_expr:
	apply_expr simple_expr		{ Application($1, $2) }
	| simple_expr				{ $1 }

simple_expr:
	| LPAREN expression RPAREN	{ $2 }
	| IDENTIFIER				{ Var($1) }
	| INTEGER					{ Integer($1) }
	| BOOL						{ Boolean($1) }

type:
	| simple_type ARROW type	{ Function($1, $3) }
	| simple_type				{ $1 }

simple_type:
	| LPAREN type RPAREN		{ $2 }
	| INT						{ Int }
