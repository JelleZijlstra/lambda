%{
	open Ast
%}

%token BACKSLASH DOT LPAREN RPAREN IDENTIFIER EOF INTEGER PLUS LET IN EQUALS
%token TIMES PRINT INT ARROW COLON FIX REC IF THEN ELSE GREATER LESS BOOL MINUS
%token COMMA FST SND UNIT BOOLEAN CASE OF BAR INL INR SEMICOLON BANG ASSIGN REF
%token LBRACE RBRACE TYPE CONSTRUCTOR MATCH WITH UNDERSCORE DATA MODULE OPEN
%token INTERFACE IMPORT END DOUBLESEMICOLON

%type<Ast.expr> expression simple_expr apply_expr plus_expr times_expr program
%type<Ast.expr> single_expr
%type<Ast.adt_cons> adt_member
%type<Ast.adt> adt_list
%type<Ast.pattern> pattern
%type<(Ast.pattern * Ast.expr) list> pattern_list
%type<string> IDENTIFIER CONSTRUCTOR
%type<int> INTEGER
%type<Ast.ltype> type
%type<bool> BOOLEAN

%start program
%%
program:
	| module_body				{ Module(None, $1) }
	| module_type inner_module_body
								{ Module(Some $1, $2) }

expression:
	single_expr SEMICOLON expression
								{ Sequence($1, $3) }
	| single_expr				{ $1 }


single_expr:
	| BACKSLASH IDENTIFIER DOT single_expr
								{ Abstraction($2, None, $4) }
	| BACKSLASH IDENTIFIER COLON type DOT single_expr
								{ Abstraction($2, Some $4, $6) }
	| in_expr IN single_expr	{ In($1, $3) }
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
	| MATCH expression WITH LBRACE pattern ARROW single_expr pattern_list RBRACE
								{ Match($2, ($5, $7)::$8) }
	| MATCH expression WITH LBRACE BAR pattern ARROW single_expr pattern_list RBRACE
								{ Match($2, ($6, $8)::$9) }
	| assign_expr				{ $1 }

in_expr:
	| LET IDENTIFIER EQUALS expression
								{ Let($2, None, $4) }
	| LET IDENTIFIER COLON type EQUALS expression
								{ Let($2, Some $4, $6) }
	| LET REC IDENTIFIER EQUALS expression
								{ LetRec($3, None, $5) }
	| LET REC IDENTIFIER COLON type EQUALS expression
								{ LetRec($3, Some $5, $7) }
	| DATA IDENTIFIER parameter_list EQUALS adt_member adt_list
								{ LetADT($2, $3, $5::$6) }
	| TYPE IDENTIFIER EQUALS type
								{ TypeSynonym($2, $4) }
	| OPEN IDENTIFIER			{ Open $2 }
	| IMPORT IDENTIFIER			{ Import $2 }

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
	| member_expr DOT IDENTIFIER
								{ Member($1, $3) }
	| member_expr DOT CONSTRUCTOR
								{ ConstructorMember($1, $3) }
	| simple_expr				{ $1 }

simple_expr:
	| LPAREN expression RPAREN	{ $2 }
	| IDENTIFIER				{ Var($1) }
	| CONSTRUCTOR				{ Constructor($1) }
	| INTEGER					{ Int($1) }
	| BOOLEAN					{ Bool($1) }
	| LPAREN expression COMMA expression RPAREN
								{ Pair($2, $4) }
	| LPAREN RPAREN				{ Unit }
	| LBRACE RBRACE				{ Record VarMap.empty }
	| LBRACE record_list RBRACE	{ Record $2 }
	| MODULE module_body END	{ Module(None, $2) }
	| MODULE module_type module_body END
								{ Module(Some $2, $3) }

module_body:
	| expression				{ [SingleExpression $1] }
	| inner_module_body			{ $1 }

inner_module_body:
	|							{ [] }
	| module_entry inner_module_body
								{ $1::$2 }

module_entry:
	in_expr						{ $1 }
	| DOUBLESEMICOLON expression
								{ SingleExpression $2 }

record_list:
	| IDENTIFIER EQUALS expression
								{ VarMap.singleton $1 $3 }
	| IDENTIFIER EQUALS expression COMMA record_list
								{ VarMap.add $1 $3 $5 }

type:
	| product_type ARROW type	{ TFunction($1, $3) }
	| product_type				{ $1 }

product_type:
	| instantiated_type TIMES product_type
								{ TProduct($1, $3) }
	| instantiated_type			{ $1 }

instantiated_type:
	| instantiated_type simple_type
								{ TParameterized($1, $2) }
	| simple_type				{ $1 }

simple_type:
	| LPAREN type RPAREN		{ $2 }
	| LBRACE RBRACE				{ TRecord VarMap.empty }
	| LBRACE record_type_list RBRACE
								{ TRecord $2 }
	| INT						{ TInt }
	| BOOL						{ TBool }
	| UNIT						{ TUnit }
	| IDENTIFIER				{ Typevar $1 }
	| module_type				{ $1 }

module_type:
	| INTERFACE LBRACE module_type_body RBRACE
								{ TModule $3 }

module_type_body:
	| module_type_entry			{ [$1] }
	| module_type_entry COMMA module_type_body
								{ $1::$3 }

module_type_entry:
	| DATA IDENTIFIER parameter_list EQUALS adt_member adt_list
								{ ConcreteType($2, $3, TADT($5::$6)) }
	| TYPE IDENTIFIER parameter_list
								{ AbstractType($2, $3) }
	| IDENTIFIER COLON type		{ Value($1, $3) }

record_type_list:
	| IDENTIFIER COLON type		{ VarMap.singleton $1 $3 }
	| IDENTIFIER COLON type COMMA record_type_list
								{ VarMap.add $1 $3 $5 }

adt_member:
	| CONSTRUCTOR type_list		{ ($1, List.rev $2) }

type_list:
	| 							{ [] }
	| simple_type type_list		{ $1::$2 }

adt_list:
	| 							{ [] }
	| BAR adt_member adt_list	{ $2::$3 }

pattern:
	| simple_pattern			{ $1 }
	| pattern simple_pattern
								{ PApplication($1, $2) }

simple_pattern:
	| UNDERSCORE				{ PAnything }
	| IDENTIFIER				{ PVariable $1 }
	| INTEGER					{ PInt $1 }
	| BOOLEAN					{ PBool $1 }
	| CONSTRUCTOR				{ PConstructor $1 }
	| LPAREN pattern COMMA pattern RPAREN
								{ PPair($2, $4) }
	| LPAREN pattern RPAREN		{ $2 }

pattern_list:
	|							{ [] }
	| BAR pattern ARROW expression pattern_list
								{ ($2, $4)::$5 }

parameter_list:
	|							{ [] }
	| IDENTIFIER parameter_list	{ $1::$2 }