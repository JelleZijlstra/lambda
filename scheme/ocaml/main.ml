(* Interpreter for the simple lambda calculus *)
let library_file =
    let scheme_root = try
        Sys.getenv "SCHEME_ROOT"
    with Not_found -> "../"
    in
    scheme_root ^ "/common/library.scm"

let main () =
	let file = ref "" in
    let verbose = ref false in

	let arguments = [
		("file", Arg.Rest (fun str -> file := str), "File to run");
        ("-v", Arg.Set verbose, "Be verbose");
	] in
	Arg.parse arguments (fun str -> file := str) "OCaml Scheme";

	let com = Util.parse_file (!file) in
	if !verbose then Printf.printf "AST: %s\n" (Ast.string_of_expr com);
    let library = Library.library in
    let scheme_library = Util.parse_file library_file in
    let _ = Eval.eval scheme_library library in
    let _ = Eval.eval com library in
    ()

let _ = main();;
