(* Interpreter for the simple lambda calculus *)
if Array.length Sys.argv <> 2 then
	(Printf.printf "Usage: %s file\n" Sys.argv.(0); exit 1);;

let com = Util.parse_file (Sys.argv.(1)) in
let result = Eval.eval com in
Printf.printf "Result: %s\n" (Ast.string_of_expr result)
