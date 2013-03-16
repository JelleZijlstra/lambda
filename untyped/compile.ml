open Ast

let translate_var v =
	"v" ^ Str.global_replace (Str.regexp "'") "_u" v

let rec compile_rec e = match e with
	| Var x -> translate_var x
	| Application(e1, e2) -> "(" ^ compile_rec e1 ^ "(" ^ compile_rec e2 ^ "))"
	| Abstraction(arg, body) -> "(function(" ^ translate_var arg ^ ") {return (" ^ compile_rec body ^ ");})"
	| Integer n -> string_of_int n
	| Binop(op, e1, e2) -> "(" ^ compile_rec e1 ^ string_of_binop op ^ compile_rec e2 ^ ")"
	| Unop(Print, e) -> "((function(x) {console.log(x);return x;})(" ^ compile_rec e ^ "))"

let compile e = "console.log(\"Result: \" + " ^ compile_rec e ^ ");"
