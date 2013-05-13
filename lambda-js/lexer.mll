{
	open Parser
}

rule token = parse
| "true"	{ BOOL true }
| "false"	{ BOOL false }
| "null"	{ NULL }
| "undefined"
			{ UNDEFINED }
| "function"
			{ FUNC }
| "func"	{ FUNC }
| "return"	{ RETURN }
| "let"		{ LET }
| "in"		{ IN }
| "delete"	{ DELETE }
| "ref"		{ REF }
| "deref"	{ DEREF }
| "if"		{ IF }
| "else"	{ ELSE }
| "while"	{ WHILE }
| "break"	{ BREAK }
| "try"		{ TRY }
| "catch"	{ CATCH }
| "finally"	{ FINALLY }
| "err"		{ ERR }
| "log"		{ LOG }
| ";"		{ SEMICOLON }
| "{"		{ LBRACE }
| "}"		{ RBRACE }
| ","		{ COMMA }
| ":"		{ COLON }
| "="		{ EQUALS }
| "["		{ LBRACKET }
| "]"		{ RBRACKET }
| "("		{ LPAREN }
| ")"		{ RPAREN }
| "+"		{ PLUS }
| "*"		{ TIMES }
| "-"		{ MINUS }
| "/"		{ SLASH }
| "=="		{ DOUBLEEQUALS }
| "!="		{ NEQUALS }
| ">"		{ GT }
| ">="		{ GTE }
| "<"		{ LT }
| "<="		{ LTE }
| "++"		{ CONCAT }
| ['A'-'Z' 'a'-'z']['A'-'Z' '_' 'a'-'z' '\'' '0'-'9']* as n
			{ IDENTIFIER(n) } (* variable names *)
| ['0'-'9']+ as n
			{ INTEGER(int_of_string n)}
| '"' ([^'"']* as s) '"'
			{ STRING s }
| [' ' '\t' '\n']
			{ token lexbuf } (* ignore whitespace *)
| "//" [^'\n']+
			{ token lexbuf} (* comments *)
| eof		{ EOF }
| _ as c
			{
				Printf.printf "Unrecognized character: [%c]\n" c;
				exit 1
			}
