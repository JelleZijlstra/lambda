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
| ","	{ COMMA }
| "let"	{ LET}
| "in"	{ IN }
| "="	{ EQUALS }
| "int" { INT }
| "bool"
		{ BOOL }
| "unit"
		{ UNIT }
| "->"	{ ARROW }
| ":"	{ COLON }
| "print"
		{ PRINT }
| "fix"	{ FIX }
| "rec"	{ REC }
| "true"
		{ BOOLEAN true }
| "false"
		{ BOOLEAN false }
| "if"	{ IF }
| "then"
		{ THEN }
| "else"
		{ ELSE }
| "fst"	{ FST }
| "snd"	{ SND }
| "case"
		{ CASE }
| "of"	{ OF }
| "|"	{ BAR }
| "inl"	{ INL }
| "inr"	{ INR }
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
