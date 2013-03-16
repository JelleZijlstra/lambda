(* Interpreter for the simple lambda calculus *)
let main () =
	let file = ref "" in
	let do_compile = ref false in
	let do_cbn = ref false in

	let arguments = [
		("-c", Arg.Set do_compile, "Compile rather than run the program");
		("-n", Arg.Set do_cbn, "Use call-by-name semantics");
		("file", Arg.Rest (fun str -> file := str), "File to run")
	] in
	Arg.parse arguments (fun str -> file := str) "Implementation of the untyped lambda calculus";

	let com = Util.parse_file (!file) in
	if !do_compile
	then let result = Compile.compile com in
		Printf.printf "%s\n" result
	else let result = (if !do_cbn then Eval.eval_cbn else Eval.eval_cbv) com in
		Printf.printf "Result: %s\n" (Ast.string_of_expr result)

let _ = main();;
