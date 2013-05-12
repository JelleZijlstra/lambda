(* Interpreter for the simple lambda calculus *)
let main () =
	let file = ref "" in

	let arguments = [
		("file", Arg.Rest (fun str -> file := str), "File to run")
	] in
	Arg.parse arguments (fun str -> file := str) "Implementation of the join calculus";

	let com = Util.parse_file (!file) in
	let result = Eval.eval com in
		Printf.printf "Result: %s\n" ""

let _ = main();;
