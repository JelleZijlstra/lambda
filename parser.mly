%{
	open Ast
%}

%token BACKSLASH DOT LPAREN RPAREN IDENTIFIER EOF INTEGER PLUS LET IN EQUALS

%type<Ast.expr> expression simple_expr apply_expr plus_expr
%type<string> IDENTIFIER
%type<int> INTEGER

%start expression
%%

expression:
	| BACKSLASH IDENTIFIER DOT expression
								{ Abstraction($2, $4) }
	| LET IDENTIFIER EQUALS expression IN expression
								{ Application(Abstraction($2, $6), $4) }
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
