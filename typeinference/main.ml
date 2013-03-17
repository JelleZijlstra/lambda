(* Interpreter for the simple lambda calculus *)
let main () =
	let file = ref "" in
	let do_compile = ref false in
	let do_compile_to_ml = ref false in
	let do_cbn = ref false in

	let arguments = [
		("-c", Arg.Set do_compile, "Compile rather than run the program");
		("-m", Arg.Set do_compile_to_ml, "Compile to OCaml");
		("-n", Arg.Set do_cbn, "Use call-by-name semantics");
		("file", Arg.Rest (fun str -> file := str), "File to run")
	] in
	Arg.parse arguments (fun str -> file := str) "Implementation of the untyped lambda calculus";

	let com = Util.parse_file (!file) in
	match Typecheck.typecheck com with
	| None ->
		if !do_compile
		then let result = Compile.compile com in
			Printf.printf "%s\n" result
		else if !do_compile_to_ml
		then let result = Compile.compile_ml com in
			Printf.printf "%s\n" result
		else let result = (if !do_cbn then Eval.eval_cbn else Eval.eval_cbv) com in
			Printf.printf "Result: %s\n" (Ast.string_of_expr result)
	| Some error ->
		Printf.printf "Error: %s\n" error; exit 1
;;

let _ = main();;
