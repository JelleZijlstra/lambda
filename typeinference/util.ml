exception FileNotFound of string

let parse_file filename =
	let file = try open_in filename
		with _ -> raise(FileNotFound filename)
	in
	let lexbuf = Lexing.from_channel file in
	let com = try Parser.program Lexer.token lexbuf
				with Parsing.Parse_error ->
					let pos = lexbuf.Lexing.lex_curr_p in
					Printf.printf "Syntax error at line %d\n" pos.Lexing.pos_lnum;
					exit 1 in
	com
;;

let find_module modul =
	(* TODO: search a $PATH *)
	parse_file (modul ^ ".lam")
