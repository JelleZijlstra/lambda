%{
	open Ast
%}

%token BACKSLASH DOT LPAREN RPAREN IDENTIFIER EOF INTEGER PLUS LET IN EQUALS
%token TIMES PRINT INT ARROW COLON FIX REC IF THEN ELSE GREATER LESS BOOL MINUS
%token COMMA FST SND UNIT BOOLEAN CASE OF BAR INL INR SEMICOLON BANG ASSIGN REF
%token LBRACE RBRACE

%type<Ast.expr> expression simple_expr apply_expr plus_expr times_expr
%type<Ast.expr> single_expr
%type<(string * Ast.expr) list> record_list
%type<(string * Ast.ltype) list> record_type_list
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
								{ Let($2, new_typevar(), $4, $6) }
	| LET IDENTIFIER COLON type EQUALS expression IN single_expr
								{ Let($2, $4, $6, $8) }
	| LET REC IDENTIFIER EQUALS expression IN single_expr
								{ LetRec($3, new_typevar(), $5, $7) }
	| LET REC IDENTIFIER COLON type EQUALS expression IN single_expr
								{ LetRec($3, $5, $7, $9) }
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
	apply_expr member_expr		{ Application($1, $2) }
	| member_expr				{ $1 }

member_expr:
	member_expr DOT IDENTIFIER	{ Member($1, $3) }
	| simple_expr				{ $1 }

simple_expr:
	| LPAREN expression RPAREN	{ $2 }
	| IDENTIFIER				{ Var($1) }
	| INTEGER					{ Int($1) }
	| BOOLEAN					{ Bool($1) }
	| LPAREN expression COMMA expression RPAREN
								{ Pair($2, $4) }
	| LPAREN RPAREN				{ Unit }
	| LBRACE RBRACE				{ Record [] }
	| LBRACE record_list RBRACE	{ Record $2 }

record_list:
	| IDENTIFIER EQUALS expression
								{ [($1, $3)] }
	| IDENTIFIER EQUALS expression COMMA record_list
								{ ($1, $3)::$5 }

type:
	| product_type ARROW type	{ TFunction($1, $3) }
	| product_type				{ $1 }

product_type:
	| simple_type TIMES product_type
								{ TProduct($1, $3) }
	| simple_type				{ $1 }

simple_type:
	| LPAREN type RPAREN		{ $2 }
	| LBRACE RBRACE				{ TRecord [] }
	| LBRACE record_type_list RBRACE
								{ TRecord $2 }
	| INT						{ TInt }
	| BOOL						{ TBool }
	| UNIT						{ TUnit }

record_type_list:
	| IDENTIFIER COLON type		{ [($1, $3)] }
	| IDENTIFIER COLON type COMMA record_type_list
								{ ($1, $3)::$5 }
