%{
	open Ast
%}

%token BACKSLASH DOT LPAREN RPAREN IDENTIFIER EOF INTEGER PLUS LET IN EQUALS TIMES PRINT INT ARROW COLON

%type<Ast.expr> expression simple_expr apply_expr plus_expr times_expr
%type<string> IDENTIFIER
%type<int> INTEGER
%type<Ast.ltype> type

%start expression
%%

expression:
	| BACKSLASH IDENTIFIER COLON type DOT expression
								{ Abstraction($2, $4, $6) }
	| LET IDENTIFIER COLON type EQUALS expression IN expression
								{ Application(Abstraction($2, $4, $8), $6) }
	| PRINT expression			{ Unop(Print, $2) }
	| times_expr				{ $1 }

times_expr:
	times_expr TIMES plus_expr	{ Binop(Times, $1, $3) }
	| plus_expr					{ $1 }

plus_expr:
	apply_expr PLUS plus_expr	{ Binop(Plus, $1, $3) }
	| apply_expr				{ $1 }

apply_expr:
	apply_expr simple_expr		{ Application($1, $2) }
	| simple_expr				{ $1 }

simple_expr:
	| LPAREN expression RPAREN	{ $2 }
	| IDENTIFIER				{ Var($1) }
	| INTEGER					{ Integer($1) }

type:
	| simple_type ARROW type	{ Function($1, $3) }
	| simple_type				{ $1 }

simple_type:
	| LPAREN type RPAREN		{ $2 }
	| INT						{ Int }
