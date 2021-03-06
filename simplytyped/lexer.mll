{
	open Parser
}

rule token = parse
| "("	{ LPAREN }
| ")"	{ RPAREN }
| "\\"	{ BACKSLASH }
| "."	{ DOT }
| "+"	{ PLUS }
| "-"	{ MINUS }
| "*"	{ TIMES }
| "let"	{ LET}
| "in"	{ IN }
| "="	{ EQUALS }
| "int" { INT }
| "->"	{ ARROW }
| ":"	{ COLON }
| "print"
		{ PRINT }
| "fix"	{ FIX }
| "rec"	{ REC }
| "true"
		{ BOOL true }
| "false"
		{ BOOL false }
| "if"	{ IF }
| "then"
		{ THEN }
| "else"
		{ ELSE }
| ">"	{ GREATER }
| "<"	{ LESS }
| ['A'-'Z' 'a'-'z']['A'-'Z' '_' 'a'-'z' '\'' '0'-'9']* as n
		{ IDENTIFIER(n) } (* variable names *)
| [' ' '\t' '\n']
		{ token lexbuf } (* ignore whitespace *)
| "#" [^'\n']+
		{ token lexbuf} (* comments *)
| eof	{ EOF }
| ['0'-'9']+ as n
		{ INTEGER(int_of_string n)}
| _ as c
		{
			Printf.printf "Unrecognized character: [%c]\n" c;
			exit 1
		}
