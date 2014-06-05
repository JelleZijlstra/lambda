%{
	open Ast

	let ut (e : expr) : typed_expr = (e, ref None)

	let rec desugar_let parameter_list body = match parameter_list with
		| [] -> body
		| (name, t)::tl -> ut(Abstraction(name, ref t, desugar_let tl body))

	let rec typed_desugar_let parameter_list body body_t = match parameter_list with
		| [] -> body, body_t
		| (name, None)::tl ->
			let rest, rest_t = typed_desugar_let tl body body_t in
			let tv = Ast.new_typevar() in
			Abstraction(name, ref(Some tv), (rest, ref(Some rest_t))), TFunction(tv, rest_t)
		| (name, Some t)::tl ->
			let rest, rest_t = typed_desugar_let tl body body_t in
			Abstraction(name, ref(Some t), (rest, ref(Some rest_t))), TFunction(t, rest_t)

%}

%token BACKSLASH DOT LPAREN RPAREN IDENTIFIER EOF INTEGER PLUS LET IN EQUALS
%token TIMES INT ARROW COLON FIX REC IF THEN ELSE GREATER LESS BOOL MINUS
%token COMMA FST SND UNIT BOOLEAN CASE OF BAR INL INR SEMICOLON BANG ASSIGN REF
%token LBRACE RBRACE TYPE CONSTRUCTOR MATCH WITH UNDERSCORE DATA MODULE OPEN
%token INTERFACE IMPORT END DOUBLESEMICOLON WHEN PERCENT SLASH AS FORALL STRING STRING_T

%type<Ast.typed_expr> expression simple_expr apply_expr plus_expr times_expr program
%type<Ast.typed_expr> single_expr
%type<Ast.adt_cons> adt_member
%type<Ast.adt> adt_list
%type<Ast.pattern> pattern
%type<(Ast.pattern * Ast.typed_expr) list> pattern_list
%type<string> IDENTIFIER CONSTRUCTOR STRING
%type<int> INTEGER
%type<Ast.ltype> type
%type<bool> BOOLEAN

%start program
%%
program:
	| module_body				{ ut(Module(None, $1)) }
	| module_type inner_module_body
								{ ut(Module(Some $1, $2)) }

expression:
	single_expr SEMICOLON expression
								{ ut(Sequence($1, $3)) }
	| single_expr				{ $1 }


single_expr:
	| BACKSLASH IDENTIFIER DOT single_expr
								{ ut(Abstraction($2, ref None, $4)) }
	| BACKSLASH IDENTIFIER COLON type DOT single_expr
								{ ut(Abstraction($2, ref(Some $4), $6)) }
	| in_expr IN single_expr	{ ut(In($1, $3)) }
	| FST single_expr			{ ut(Projection(false, $2)) }
	| SND single_expr			{ ut(Projection(true, $2)) }
	| IF expression THEN expression ELSE single_expr
								{ ut(If($2, $4, $6)) }
	| CASE expression OF expression BAR single_expr
								{ ut(Case($2, $4, $6)) }
	| INL single_expr			{ ut(Injection(false, $2)) }
	| INR single_expr			{ ut(Injection(true, $2)) }
	| FIX single_expr			{ ut(Fix($2)) }
	| BANG single_expr			{ ut(Dereference($2)) }
	| REF single_expr			{ ut(Allocation($2)) }
	| MATCH expression WITH LBRACE pattern ARROW single_expr pattern_list RBRACE
								{ ut(Match($2, ($5, $7)::$8)) }
	| MATCH expression WITH LBRACE BAR pattern ARROW single_expr pattern_list RBRACE
								{ ut(Match($2, ($6, $8)::$9)) }
	| assign_expr				{ $1 }

in_expr:
	| LET IDENTIFIER possibly_typed_parameter_list EQUALS expression
								{ Let($2, ref None, desugar_let $3 $5) }
	| LET IDENTIFIER possibly_typed_parameter_list COLON type EQUALS expression
								{
									let value, t = typed_desugar_let $3 (Wrapped $7) $5 in
									let t' = ref (Some t) in
									Let($2, t', (value, t'))
								}
	| LET REC IDENTIFIER possibly_typed_parameter_list EQUALS expression
								{ LetRec($3, ref None, desugar_let $4 $6) }
	| LET REC IDENTIFIER possibly_typed_parameter_list COLON type EQUALS expression
								{
									let value, t = typed_desugar_let $4 (Wrapped $8) $6 in
									let t' = ref (Some t) in
									LetRec($3, t', (value, t'))
								}
	| DATA IDENTIFIER parameter_list
								{ LetADT($2, $3, []) }
	| DATA IDENTIFIER parameter_list EQUALS adt_member adt_list
								{ LetADT($2, $3, $5::$6) }
	| TYPE IDENTIFIER EQUALS type
								{ TypeSynonym($2, $4) }
	| OPEN IDENTIFIER			{ Open $2 }
	| IMPORT IDENTIFIER			{ Import $2 }

assign_expr:
	equals_expr ASSIGN equals_expr
								{ ut(Assignment($1, $3)) }
	| equals_expr				{ $1 }

equals_expr:
	plus_expr EQUALS plus_expr	{ ut(Boolbinop(Equals, $1, $3)) }
	| plus_expr GREATER plus_expr
								{ ut(Boolbinop(Greater, $1, $3)) }
	| plus_expr LESS plus_expr	{ ut(Boolbinop(Less, $1, $3)) }
	| plus_expr					{ $1 }

plus_expr:
	times_expr PLUS plus_expr	{ ut(Binop(Plus, $1, $3)) }
	| times_expr MINUS plus_expr
								{ ut(Binop(Minus, $1, $3)) }
	| times_expr				{ $1 }

times_expr:
	apply_expr TIMES times_expr	{ ut(Binop(Times, $1, $3)) }
	| apply_expr SLASH times_expr
								{ ut(Binop(Divide, $1, $3)) }
	| apply_expr PERCENT times_expr
								{ ut(Binop(Modulo, $1, $3)) }
	| apply_expr				{ $1 }

apply_expr:
	apply_expr member_expr		{ ut(Application($1, $2)) }
	| member_expr				{ $1 }

member_expr:
	| member_expr DOT IDENTIFIER
								{ ut(Member($1, $3)) }
	| member_expr DOT CONSTRUCTOR
								{ ut(ConstructorMember($1, $3)) }
	| simple_expr				{ $1 }

simple_expr:
	| LPAREN expression RPAREN	{ $2 }
	| IDENTIFIER				{ ut(Var($1)) }
	| CONSTRUCTOR				{ ut(Constructor($1)) }
	| INTEGER					{ ut(Int($1)) }
	| BOOLEAN					{ ut(Bool($1)) }
	| STRING					{ ut(String($1)) }
	| LPAREN expression COMMA expression RPAREN
								{ ut(Pair($2, $4)) }
	| LPAREN RPAREN				{ ut(Unit) }
	| LBRACE RBRACE				{ ut(Record VarMap.empty) }
	| LBRACE record_list RBRACE	{ ut(Record $2) }
	| MODULE module_body END	{ ut(Module(None, $2)) }
	| MODULE module_type module_body END
								{ ut(Module(Some $2, $3)) }

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
	| FORALL IDENTIFIER parameter_list DOT arrow_type
								{ TForAll($2::$3, $5) }
	| arrow_type				{ $1 }

arrow_type:
	| product_type ARROW arrow_type
								{ TFunction($1, $3) }
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
	| STRING_T					{ TString }
	| IDENTIFIER				{ TNamedType $1 }
	| module_type				{ $1 }

module_type:
	| INTERFACE LBRACE module_type_body RBRACE
								{ TModule $3 }

module_type_body:
	| module_type_entry			{ [$1] }
	| module_type_entry COMMA module_type_body
								{ $1::$3 }

module_type_entry:
	| DATA IDENTIFIER parameter_list
								{ ConcreteType($2, TForAll($3, TADT [])) }
	| DATA IDENTIFIER parameter_list EQUALS adt_member adt_list
								{ ConcreteType($2, TForAll($3, TADT($5::$6))) }
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
	| as_pattern				{ $1 }
	| as_pattern WHEN expression
								{ PGuarded($1, $3) }

as_pattern:
	apply_pattern AS IDENTIFIER	{ PAs($1, $3) }
	| apply_pattern				{ $1 }

apply_pattern:
	| simple_pattern			{ $1 }
	| apply_pattern simple_pattern
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

possibly_typed_parameter_list:
	|							{ [] }
	| IDENTIFIER possibly_typed_parameter_list
								{ ($1, None)::$2 }
	| LPAREN IDENTIFIER COLON type RPAREN possibly_typed_parameter_list
								{ ($2, Some $4)::$6 }

parameter_list:
	|							{ [] }
	| IDENTIFIER parameter_list	{ $1::$2 }
