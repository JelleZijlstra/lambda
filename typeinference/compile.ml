open Ast

let z =
	let inner_z = Abstraction("x", Int, Application(Var "f", Abstraction("y", Int, Application(Application(Var "x", Var "x"), Var "y")))) in
	Abstraction("f", Int, Application(inner_z, inner_z))

let translate_var v =
	"v" ^ Str.global_replace (Str.regexp "'") "_u" v

let rec compile_rec e = match e with
	| Var x -> translate_var x
	| Application(e1, e2) -> "(" ^ compile_rec e1 ^ "(" ^ compile_rec e2 ^ "))"
	| Abstraction(arg, _, body) -> "(function(" ^ translate_var arg ^ ") {return (" ^ compile_rec body ^ ");})"
	| Integer n -> string_of_int n
	| Binop(op, e1, e2) -> "(" ^ compile_rec e1 ^ string_of_binop op ^ compile_rec e2 ^ ")"
	| Boolbinop(Equals, e1, e2) -> "(" ^ compile_rec e1 ^ " == " ^ compile_rec e2 ^ ")"
	| Boolbinop(op, e1, e2) -> "(" ^ compile_rec e1 ^ string_of_bool_binop op ^ compile_rec e2 ^ ")"
	| Unop(Print, e) -> "((function(x) {console.log(x);return x;})(" ^ compile_rec e ^ "))"
	| Fix(Abstraction(arg, t, body)) ->
		(* Apply the Z combinator *)
		compile_rec(Application(z, Abstraction(arg, t, body)))
	| Fix _ -> failwith "Impossible"
	| If(e1, e2, e3) -> "((" ^ compile_rec e1 ^ ") ? (" ^ compile_rec e2 ^ ") : (" ^ compile_rec e3 ^ "))"
	| Boolean true -> "true"
	| Boolean false -> "false"

let compile e = "console.log(\"Result: \" + " ^ compile_rec e ^ ");"
