%{
	open Ast
%}

%token BACKSLASH DOT LPAREN RPAREN IDENTIFIER EOF

%type<Ast.expr> expression simple_expr apply_expr
%type<string> IDENTIFIER


%start expression
%%

expression:
	| BACKSLASH IDENTIFIER DOT expression { Abstraction($2, $4) }
	| apply_expr	{ $1 }

apply_expr:
	apply_expr simple_expr { Application($1, $2) }
	| simple_expr { $1 }

simple_expr:
	| LPAREN expression RPAREN { $2 }
	| IDENTIFIER { Var($1) }
