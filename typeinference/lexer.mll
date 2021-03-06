{
	open Parser
}

rule token = parse
| "("	{ LPAREN }
| ")"	{ RPAREN }
| "{"	{ LBRACE }
| "}"	{ RBRACE }
| ";"	{ SEMICOLON }
| "\\"	{ BACKSLASH }
| "."	{ DOT }
| "+"	{ PLUS }
| "-"	{ MINUS }
| "*"	{ TIMES }
| "/"	{ SLASH }
| "%"	{ PERCENT }
| ","	{ COMMA }
| "ref"	{ REF }
| "!"	{ BANG }
| ":="	{ ASSIGN }
| "let"	{ LET}
| "in"	{ IN }
| "="	{ EQUALS }
| "int" { INT }
| "bool"
		{ BOOL }
| "unit"
		{ UNIT }
| "string"
		{ STRING_T }
| "->"	{ ARROW }
| ":"	{ COLON }
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
| "reveal_type"
		{ REVEAL_TYPE }
| "case"
		{ CASE }
| "of"	{ OF }
| "|"	{ BAR }
| "inl"	{ INL }
| "inr"	{ INR }
| "type"
		{ TYPE }
| "data"
		{ DATA }
| "forall"
		{ FORALL }
| "match"
		{ MATCH }
| "with"
		{ WITH }
| "when"
		{ WHEN }
| "as"	{ AS }
| "module"
		{ MODULE }
| "interface"
		{ INTERFACE }
| "import"
		{ IMPORT }
| "open"
		{ OPEN }
| "end"	{ END }
| ";;"	{ DOUBLESEMICOLON }
| "_"	{ UNDERSCORE }
| ">"	{ GREATER }
| "<"	{ LESS }
| ['a'-'z']['A'-'Z' '_' 'a'-'z' '\'' '0'-'9']* as n
		{ IDENTIFIER(n)  (* variable names *) }
| ['A'-'Z']['A'-'Z' '_' 'a'-'z' '\'' '0'-'9']* as n
		{ CONSTRUCTOR(n) }
| '"'[^'"']*'"' as s
		{ STRING (String.sub s 1 (String.length s - 2)) }
| [' ' '\t' '\n']
		{ token lexbuf (* ignore whitespace *) }
| "#" [^'\n']+
		{ token lexbuf  (* comments *) }
| eof	{ EOF }
| ['0'-'9']+ as n
		{ INTEGER(int_of_string n)}
| _ as c
		{
			Printf.printf "Unrecognized character: [%c]\n" c;
			exit 1
		}
