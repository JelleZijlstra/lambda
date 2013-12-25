{
	open Parser
}

rule token = parse
| "("	{ LPAREN }
| ")"	{ RPAREN }
| "."	{ DOT }
| "'"	{ QUOTE }
| [' ' '\t' '\n']
		{ token lexbuf } (* ignore whitespace *)
| "#t"
		{ BOOL true }
| "#f"	{ BOOL false }
| ";" [^'\n']+ { token lexbuf } (* ignore comments *)
| '"'[^'"']*'"' as s
		{ STRING (String.sub s 1 (String.length s - 2)) }
| ['0'-'9']+ as n
		{ INTEGER(int_of_string n)}
| [^'0'-'9' '(' ' ' '\t' '\n' ';' ')' '.' '\''] [^ ' ' '\t' '\n' ')' '(']* as n
		{ IDENTIFIER n } (* variable names *)
| eof	{ EOF }
| _ as c
		{
			Printf.printf "Unrecognized character: [%c]\n" c;
			exit 1
		}
