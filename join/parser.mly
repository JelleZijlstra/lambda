%{
	open Ast
%}

%token DEF IN LT GT CARET RBRACE NULL IDENTIFIER INTEGER EOF BAR COMMA LPAREN RPAREN PRINT

%type<string> IDENTIFIER
%type<int> INTEGER
%type<Ast.process> process

%start process
%%
process:
	| simple_process
					{ $1 }
	| simple_process BAR process
					{ PCompose($1, $3) }
	| DEF definition IN process
					{ PDef($2, $4) }

simple_process:
	| IDENTIFIER LT message GT
					{ PSend($1, $3) }
	| NULL			{ PNull }
	| LPAREN process RPAREN
					{ $2 }
	| PRINT INTEGER	{ PPrint $2 }

message:
	|				{ [] }
	| IDENTIFIER inner_message
					{ $1::$2 }

inner_message:
	|				{ [] }
	| COMMA IDENTIFIER inner_message
					{ $2::$3 }

definition:
	| simple_definition
					{ $1 }
	| simple_definition CARET definition
					{ DConjunction($1, $3) }

simple_definition:
	| join_pattern RBRACE process
					{ DDefined($1, $3) }

join_pattern:
	| simple_join_pattern
					{ $1 }
	| simple_join_pattern BAR join_pattern
					{ JConjunction($1, $3) }

simple_join_pattern:
	| IDENTIFIER LT message GT
					{ JReceive($1, $3) }
