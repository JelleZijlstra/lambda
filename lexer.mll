{
	open Parser
}

rule token = parse
| "("	{ LPAREN }
| ")"	{ RPAREN }
| "\\"	{ BACKSLASH }
| "."	{ DOT }
| ['A'-'Z''a'-'z']['A'-'Z''a'-'z''\'''0'-'9']* as n
		{ IDENTIFIER(n) } (* variable names *)
| [' ' '\t' '\n']
		{ token lexbuf } (* ignore whitespace *)
| "#" [^'\n']+
		{ token lexbuf} (* comments *)
| eof	{ EOF }
| _ as c
		{
			Printf.printf "Unrecognized character: [%c]\n" c;
			exit 1
		}
