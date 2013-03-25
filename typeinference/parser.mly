%{
	open Ast
%}

%token BACKSLASH DOT LPAREN RPAREN IDENTIFIER EOF INTEGER PLUS LET IN EQUALS
%token TIMES PRINT INT ARROW COLON FIX REC IF THEN ELSE GREATER LESS BOOL MINUS
%token COMMA FST SND UNIT BOOLEAN CASE OF BAR INL INR SEMICOLON BANG ASSIGN REF

%type<Ast.expr> expression simple_expr apply_expr plus_expr times_expr single_expr
%type<string> IDENTIFIER
%type<int> INTEGER
%type<Ast.ltype> type
%type<bool> BOOLEAN

%start expression
%%
expression:
	single_expr SEMICOLON expression
								{ Sequence($1, $3) }
	| single_expr				{ $1 }


single_expr:
	| BACKSLASH IDENTIFIER DOT single_expr
								{ Abstraction($2, new_typevar(), $4) }
	| BACKSLASH IDENTIFIER COLON type DOT single_expr
								{ Abstraction($2, $4, $6) }
	| LET IDENTIFIER EQUALS expression IN single_expr
								{ Application(Abstraction($2, new_typevar(), $6), $4) }
	| LET IDENTIFIER COLON type EQUALS expression IN single_expr
								{ Application(Abstraction($2, $4, $8), $6) }
	| LET REC IDENTIFIER EQUALS expression IN single_expr
								{ Application(Abstraction($3, new_typevar(), $7), Fix(Abstraction($3, new_typevar(), $5))) }
	| LET REC IDENTIFIER COLON type EQUALS expression IN single_expr
								{ Application(Abstraction($3, $5, $9), Fix(Abstraction($3, $5, $7))) }
	| FST single_expr			{ Projection(false, $2) }
	| SND single_expr			{ Projection(true, $2) }
	| IF expression THEN expression ELSE single_expr
								{ If($2, $4, $6) }
	| CASE expression OF expression BAR single_expr
								{ Case($2, $4, $6) }
	| INL single_expr			{ Injection(false, $2) }
	| INR single_expr			{ Injection(true, $2) }
	| PRINT single_expr			{ Unop(Print, $2) }
	| FIX single_expr			{ Fix($2) }
	| BANG single_expr			{ Dereference($2) }
	| REF single_expr			{ Allocation($2) }
	| assign_expr				{ $1 }

assign_expr:
	equals_expr ASSIGN equals_expr
								{ Assignment($1, $3) }
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
	| BOOLEAN					{ Boolean($1) }
	| LPAREN expression COMMA expression RPAREN
								{ Pair($2, $4) }
	| LPAREN RPAREN				{ Unit }

type:
	| product_type ARROW type	{ Function($1, $3) }
	| product_type				{ $1 }

product_type:
	| simple_type TIMES product_type
								{ Product($1, $3) }
	| simple_type				{ $1 }

simple_type:
	| LPAREN type RPAREN		{ $2 }
	| INT						{ Int }
	| BOOL						{ Bool }
	| UNIT						{ Uni }
