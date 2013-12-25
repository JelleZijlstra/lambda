(* Interpreter for the simple lambda calculus *)
let main () =
	let file = ref "" in

	let arguments = [
		("file", Arg.Rest (fun str -> file := str), "File to run")
	] in
	Arg.parse arguments (fun str -> file := str) "OCaml Scheme";

	let com = Util.parse_file (!file) in
	Printf.printf "AST: %s\n" (Ast.string_of_expr com);
    let library = Library.library in
	let result = Eval.eval com library in
		Printf.printf "Result: %s\n" (Ast.string_of_expr result)

let _ = main();;
