if Array.length Sys.argv <> 2 then
	(Printf.printf "Usage: %s file\n" Sys.argv.(0); exit 1);;

let file = open_in (Sys.argv.(1)) in
let lexbuf = Lexing.from_channel file in
let com = try Parser.expression Lexer.token lexbuf
			with Parsing.Parse_error ->
				let pos = lexbuf.Lexing.lex_curr_p in
				Printf.printf "Syntax error at line %d\n" pos.Lexing.pos_lnum;
				exit 1 in
let result = Eval.eval com in
Printf.printf "%s\n" (Ast.stringify result)