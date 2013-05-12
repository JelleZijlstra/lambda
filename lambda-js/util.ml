let parse_file filename =
	let file = open_in filename in
	let lexbuf = Lexing.from_channel file in
	let com = try Parser.expr Lexer.token lexbuf
				with Parsing.Parse_error ->
					let pos = lexbuf.Lexing.lex_curr_p in
					Printf.printf "Syntax error at line %d\n" pos.Lexing.pos_lnum;
					exit 1 in
	com
;;
