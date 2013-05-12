%{
	open Ast
%}

%token INTEGER STRING BOOL NULL UNDEFINED FUNC LPAREN RPAREN LBRACE RETURN
%token RBRACE IDENTIFIER LET EQUALS IN DELETE COMMA COLON LBRACKET RBRACKET
%token EOF REF DEREF IF ELSE WHILE BREAK TRY CATCH FINALLY ERR SEMICOLON
%token LOG PLUS MINUS TIMES SLASH LT LTE GT GTE DOUBLEEQUALS NEQUALS CONCAT

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
	| compare_expr	{ $1 }
	| access_expr LPAREN expr_list RPAREN
					{ Call($1, $3) }
	| access_expr LBRACKET expr RBRACKET
					{ Access($1, $3) }
	| access_expr LBRACKET expr RBRACKET EQUALS compare_expr
					{ Assignment($1, $3, $6) }
	| compare_expr EQUALS compare_expr
					{ SetRef($1, $3) }

compare_expr:
	| plus_expr DOUBLEEQUALS plus_expr
					{ Binop(Equals, $1, $3) }
	| plus_expr NEQUALS plus_expr
					{ Binop(NEquals, $1, $3) }
	| plus_expr LT plus_expr
					{ Binop(Less, $1, $3) }
	| plus_expr LTE plus_expr
					{ Binop(LE, $1, $3) }
	| plus_expr GT plus_expr
					{ Binop(Greater, $1, $3) }
	| plus_expr GTE plus_expr
					{ Binop(GE, $1, $3) }
	| plus_expr	{ $1 }

plus_expr:
	| times_expr PLUS plus_expr
					{ Binop(Add, $1, $3) }
	| times_expr MINUS plus_expr
					{ Binop(Subtract, $1, $3) }
	| times_expr CONCAT plus_expr
					{ Binop(Concat, $1, $3) }
	| times_expr	{ $1 }

times_expr:
	| simple_expr TIMES times_expr
					{ Binop(Multiply, $1, $3) }
	| simple_expr SLASH times_expr
					{ Binop(Divide, $1, $3) }
	| simple_expr	{ $1 }

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
	| LOG LPAREN expr RPAREN
					{ Log $3 }

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
