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
	| Pair(e1, e2) -> "[" ^ compile_rec e1 ^ ", " ^ compile_rec e2 ^ "]"
	| Projection(false, e) -> "(" ^ compile_rec e ^ "[0])"
	| Projection(true, e) -> "(" ^ compile_rec e ^ "[1])"
	| Unit -> "null"
	| Injection(b, e) -> "[" ^ (if b then "true" else "false") ^ ", " ^ compile_rec e ^ "]"
	| Case(e1, e2, e3) -> "((function(x) { return x[0] ? (" ^ compile_rec e3 ^ "(x[1])) : ("
		^ compile_rec e2 ^ "(x[1])); })(" ^ compile_rec e1 ^ "))"

let compile e = "console.log(\"Result: \" + " ^ compile_rec e ^ ");"

let rec compile_rec e = match e with
	| Var x -> translate_var x
	| Application(e1, e2) -> "(" ^ compile_rec e1 ^ " " ^ compile_rec e2 ^ ")"
	| Abstraction(arg, _, body) -> "(fun " ^ translate_var arg ^ " -> " ^ compile_rec body ^ ")"
	| Integer n -> string_of_int n
	| Boolean true -> "true"
	| Boolean false -> "false"
	| Unit -> "()"
	| Binop(op, e1, e2) -> "(" ^ compile_rec e1 ^ string_of_binop op ^ compile_rec e2 ^ ")"
	| Boolbinop(op, e1, e2) -> "(" ^ compile_rec e1 ^ string_of_bool_binop op ^ compile_rec e2 ^ ")"
	| Unop(Print, e) -> "(let e = " ^ compile_rec e ^ " in Printf.printf \"%d\\n\" e; e)"
	| If(e1, e2, e3) -> "(if " ^ compile_rec e1 ^ " then " ^ compile_rec e2 ^ " else " ^ compile_rec e3 ^ ")"
	| Fix(Abstraction(arg, t, body)) -> "(let rec " ^ translate_var arg ^ " = " ^ compile_rec body ^ " in " ^ translate_var arg ^ ")"
	| Fix _ -> failwith "Impossible"
	| Pair(e1, e2) -> "(" ^ compile_rec e1 ^ ", " ^ compile_rec e2 ^ ")"
	| Projection(false, e) -> "(fst " ^ compile_rec e ^ ")"
	| Projection(true, e) -> "(snd " ^ compile_rec e ^ ")"
	| Injection(b, e) -> failwith "Not implemented"
	| Case(e1, e2, e3) -> failwith "Not implemented"

let compile_ml e = "let _ = Printf.printf \"%d\\n\" (" ^ compile_rec e ^ ");;"
