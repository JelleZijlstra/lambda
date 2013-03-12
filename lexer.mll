{
	open Parser
}

rule token = parse
| "("	{ LPAREN }
| ")"	{ RPAREN }
| "\\"	{ BACKSLASH }
| "."	{ DOT }
| ['A'-'Z''a'-'z']['A'-'Z''a'-'z''\'''0'-'9']* as n
		{ IDENTIFIER(n) }
| [' ' '\t' '\n']	{ token lexbuf }
| eof	{ EOF }
| _ as c  {
            Printf.printf "Unrecognized character: [%c]\n" c;
            exit 1
          }