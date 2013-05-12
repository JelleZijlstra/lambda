%{
	open Ast
%}

%token INTEGER STRING BOOL NULL UNDEFINED FUNC LPAREN RPAREN LBRACE RETURN
%token RBRACE IDENTIFIER LET EQUALS IN DELETE COMMA COLON LBRACKET RBRACKET
%token EOF REF DEREF IF ELSE WHILE BREAK TRY CATCH FINALLY ERR SEMICOLON

%type<string> IDENTIFIER STRING
%type<int> INTEGER
%type<bool> BOOL
%type<Ast.expr> expr

%start expr
%%
constant:
	INTEGER			{ CInt $1 }
	| STRING		{ CString $1 }
	| BOOL			{ CBool $1 }
	| NULL			{ CNull }
	| UNDEFINED		{ CUndefined }

value:
	constant		{ VConstant $1 }
	| FUNC LPAREN arg_list RPAREN LBRACE RETURN expr RBRACE
					{ VFunc($3, $7) }
	| LBRACE obj_list RBRACE
					{ VObject $2 }

expr:
	| let_expr SEMICOLON expr
					{ Sequence($1, $3) }
	| let_expr		{ $1 }

let_expr:
	| LET IDENTIFIER EQUALS expr IN let_expr
					{ Let($2, $4, $6) }
	| REF let_expr	{ Ref $2 }
	| DEREF let_expr
					{ Deref $2 }
	| BREAK IDENTIFIER let_expr
					{ Break($2, $3) }
	| access_expr	{ $1 }

access_expr:
	| simple_expr	{ $1 }
	| access_expr LPAREN expr_list RPAREN
					{ Call($1, $3) }
	| access_expr LBRACKET expr RBRACKET
					{ Access($1, $3) }
	| access_expr LBRACKET expr RBRACKET EQUALS simple_expr
					{ Assignment($1, $3, $6) }
	| simple_expr EQUALS simple_expr
					{ SetRef($1, $3) }

simple_expr:
	| DELETE expr LBRACKET expr RBRACKET
					{ Delete($2, $4) }
	| IDENTIFIER	{ Var $1 }
	| value			{ Value $1 }
	| LPAREN expr RPAREN
					{ $2 }
	| IF LPAREN expr RPAREN LBRACE expr RBRACE ELSE LBRACE expr RBRACE
					{ If($3, $6, $10) }
	| WHILE LPAREN expr RPAREN LBRACE expr RBRACE
					{ While($3, $6) }
	| IDENTIFIER COLON LBRACE expr RBRACE
					{ LabeledBlock($1, $4) }
	| TRY LBRACE expr RBRACE CATCH LPAREN IDENTIFIER RPAREN LBRACE expr RBRACE
					{ TryCatch($3, $7, $10) }
	| TRY LBRACE expr RBRACE FINALLY LBRACE expr RBRACE
					{ TryFinally($3, $7) }
	| ERR value		{ Err $2 }

arg_list:
	|				{ [] }
	| IDENTIFIER inner_arg_list
					{ $1::$2 }

inner_arg_list:
	|				{ [] }
	| COMMA IDENTIFIER inner_arg_list
					{ $2::$3 }

obj_list:
	|				{ VarMap.empty }
	| STRING COLON value inner_obj_list
					{ VarMap.add $1 $3 $4 }

inner_obj_list:
	| 				{ VarMap.empty }
	| COMMA STRING COLON value inner_obj_list
					{ VarMap.add $2 $4 $5 }

expr_list:
	|				{ [] }
	| expr inner_expr_list
					{ $1::$2 }

inner_expr_list:
	|				{ [] }
	| COMMA expr inner_expr_list
					{ $2::$3 }
