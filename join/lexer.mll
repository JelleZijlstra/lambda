{
	open Parser
}

rule token = parse
| "<"		{ LT }
| ">"		{ GT }
| "^"		{ CARET }
| "}"		{ RBRACE }
| "|"		{ BAR }
| ","		{ COMMA }
| "()"		{ NULL }
| "def"		{ DEF }
| "in"		{ IN }
| "print"	{ PRINT }
| "("		{ LPAREN }
| ")"		{ RPAREN }
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
