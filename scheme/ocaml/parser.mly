%{
	open Ast
%}

%token QUOTE DOT LPAREN RPAREN IDENTIFIER EOF INTEGER STRING BOOL

%type<Ast.expr> list
%type<Ast.expr list> program
%type<string> IDENTIFIER STRING
%type<int> INTEGER
%type<bool> BOOL

%start program
%%

program:
	list program	{ $1::$2 }
	|				{ [] }

list:
	LPAREN inner_list RPAREN
					{ List $2 }

inner_list:
	atom inner_list	{ $1::$2 }
	| DOT atom
					{ [Dotted $2] }
	|				{ [] }

atom:
	IDENTIFIER		{ Var $1 }
	| STRING		{ String $1 }
	| INTEGER		{ Integer $1 }
	| BOOL			{ Bool $1 }
	| list			{ $1 }
	| QUOTE atom	{ Quoted $2 }