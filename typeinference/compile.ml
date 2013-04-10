open Ast

let z =
	let inner_z = Abstraction("x", None, Application(Var "f", Abstraction("y", None, Application(Application(Var "x", Var "x"), Var "y")))) in
	Abstraction("f", None, Application(inner_z, inner_z))

let translate_var v =
	"v" ^ Str.global_replace (Str.regexp "'") "_u" v

let rec compile_rec e = match e with
	| Var x -> translate_var x
	| Application(e1, e2) -> "(" ^ compile_rec e1 ^ "(" ^ compile_rec e2 ^ "))"
	| Abstraction(arg, _, body) -> "(function(" ^ translate_var arg ^ ") {return (" ^ compile_rec body ^ ");})"
	| Int n -> string_of_int n
	| Binop(op, e1, e2) -> "(" ^ compile_rec e1 ^ string_of_binop op ^ compile_rec e2 ^ ")"
	| Boolbinop(Equals, e1, e2) -> "(" ^ compile_rec e1 ^ " == " ^ compile_rec e2 ^ ")"
	| Boolbinop(op, e1, e2) -> "(" ^ compile_rec e1 ^ string_of_bool_binop op ^ compile_rec e2 ^ ")"
	| Unop(Print, e) -> "((function(x) {console.log(x);return x;})(" ^ compile_rec e ^ "))"
	| Fix(Abstraction(arg, t, body)) ->
		(* Apply the Z combinator *)
		compile_rec(Application(z, Abstraction(arg, t, body)))
	| Fix _ | Reference _ -> failwith "Impossible"
	| If(e1, e2, e3) -> "((" ^ compile_rec e1 ^ ") ? (" ^ compile_rec e2 ^ ") : (" ^ compile_rec e3 ^ "))"
	| Bool true -> "true"
	| Bool false -> "false"
	| Pair(e1, e2) -> "[" ^ compile_rec e1 ^ ", " ^ compile_rec e2 ^ "]"
	| Projection(false, e) -> "(" ^ compile_rec e ^ "[0])"
	| Projection(true, e) -> "(" ^ compile_rec e ^ "[1])"
	| Unit -> "null"
	| Injection(b, e) -> "[" ^ (if b then "true" else "false") ^ ", " ^ compile_rec e ^ "]"
	| Case(e1, e2, e3) -> "((function(x) { return x[0] ? (" ^ compile_rec e3 ^ "(x[1])) : ("
		^ compile_rec e2 ^ "(x[1])); })(" ^ compile_rec e1 ^ "))"
	| Allocation(e) -> "{value:(" ^ compile_rec e ^ ")}"
	| Assignment(e1, e2) -> "((" ^ compile_rec e1 ^ ").value = (" ^ compile_rec e2 ^ "))"
	| Dereference e -> "(" ^ compile_rec e ^ ").value"
	| Sequence(e1, e2) -> "((function() { " ^ compile_rec e1 ^ "; return (" ^ compile_rec e2 ^ "); })())"
	| Record lst ->
		let foldf rest (l, e) = "\"" ^ l ^ "\": " ^ compile_rec e ^ ", " ^ rest in
		"{" ^ List.fold_left foldf "" lst ^ "}"
	| Member(e, l) -> "(" ^ compile_rec e ^ ")." ^ l
	| Let(x, t, e1, e2) -> compile_rec(Application(Abstraction(x, t, e2), e1))
	| LetRec(x, t, e1, e2) -> compile_rec(Application(Abstraction(x, t, e2), Fix(Abstraction(x, t, e1))))

let compile e = "console.log(\"Result: \" + " ^ compile_rec e ^ ");"

let rec compile_rec e = match e with
	| Var x -> translate_var x
	| Application(e1, e2) -> "(" ^ compile_rec e1 ^ " " ^ compile_rec e2 ^ ")"
	| Abstraction(arg, _, body) -> "(fun " ^ translate_var arg ^ " -> " ^ compile_rec body ^ ")"
	| Int n -> string_of_int n
	| Bool true -> "true"
	| Bool false -> "false"
	| Unit -> "()"
	| Binop(op, e1, e2) -> "(" ^ compile_rec e1 ^ string_of_binop op ^ compile_rec e2 ^ ")"
	| Boolbinop(op, e1, e2) -> "(" ^ compile_rec e1 ^ string_of_bool_binop op ^ compile_rec e2 ^ ")"
	| Unop(Print, e) -> "(let e = " ^ compile_rec e ^ " in Printf.printf \"%d\\n\" e; e)"
	| If(e1, e2, e3) -> "(if " ^ compile_rec e1 ^ " then " ^ compile_rec e2 ^ " else " ^ compile_rec e3 ^ ")"
	| Fix(Abstraction(arg, t, body)) -> "(let rec " ^ translate_var arg ^ " = " ^ compile_rec body ^ " in " ^ translate_var arg ^ ")"
	| Fix _ | Reference _ -> failwith "Impossible"
	| Pair(e1, e2) -> "(" ^ compile_rec e1 ^ ", " ^ compile_rec e2 ^ ")"
	| Projection(false, e) -> "(fst " ^ compile_rec e ^ ")"
	| Projection(true, e) -> "(snd " ^ compile_rec e ^ ")"
	| Injection(b, e) -> failwith "Not implemented"
	| Case(e1, e2, e3) -> failwith "Not implemented"
	| Allocation e -> "(ref " ^ compile_rec e ^ ")"
	| Assignment(e1, e2) -> "(" ^ compile_rec e1 ^ " := " ^ compile_rec e2 ^ ")"
	| Dereference e -> "(!" ^ compile_rec e ^ ")"
	| Sequence(e1, e2) -> "(ignore(" ^ compile_rec e1 ^ "); " ^ compile_rec e2 ^ ")"
	| Record lst -> failwith "Not implemented"
	| Member(e, l) -> failwith "Not implemented"
	| Let(x, t, e1, e2) -> compile_rec(Application(Abstraction(x, t, e2), e1))
	| LetRec(x, t, e1, e2) -> compile_rec(Application(Abstraction(x, t, e2), Fix(Abstraction(x, t, e1))))

let compile_ml e = "let _ = Printf.printf \"%d\\n\" (" ^ compile_rec e ^ ");;"

let make_new_var : unit -> string =
	let n = ref 0 in
	fun () ->
		let current = !n in
		n := current + 1;
		"n" ^ string_of_int current

let rec compile_rec e = match e with
	| Var x -> translate_var x
	| Application(e1, e2) -> "(" ^ compile_rec e1 ^ " " ^ compile_rec e2 ^ ")"
	| Abstraction(arg, _, body) -> "(" ^ translate_var arg ^ " => (" ^ compile_rec body ^ "))"
	| Int n -> string_of_int n
	| Bool true -> "true"
	| Bool false -> "false"
	| Unit -> "()"
	| Binop(op, e1, e2) -> "(" ^ compile_rec e1 ^ " " ^ string_of_binop op ^ " " ^ compile_rec e2 ^ ")"
	| Boolbinop(Equals, e1, e2) -> "(" ^ compile_rec e1 ^ " == " ^ compile_rec e2 ^ ")"
	| Boolbinop(op, e1, e2) -> "(" ^ compile_rec e1 ^ " " ^ string_of_bool_binop op ^ " " ^ compile_rec e2 ^ ")"
	| Unop(Print, e) -> "((func: x; echo x; x; end)" ^ compile_rec e ^ ")"
	| If(e1, e2, e3) -> "(if (" ^ compile_rec e1 ^ "); " ^ compile_rec e2 ^ "; else " ^ compile_rec e3 ^ "; end)"
	| Fix(Abstraction(arg, _, body)) ->
		"((func: ; private " ^ translate_var arg ^ " = " ^ compile_rec body ^ "; "
			^ translate_var arg ^ "; end) ())"
	| Fix _ | Reference _ -> failwith "Impossible"
	| Pair(e1, e2) -> "(" ^ compile_rec e1 ^ ", " ^ compile_rec e2 ^ ")"
	| Projection(false, e) -> "(" ^ compile_rec e ^ "->0)"
	| Projection(true, e) -> "(" ^ compile_rec e ^ "->1)"
	| Injection(b, e) -> "(" ^ (if b then "true" else "false") ^ ", " ^ compile_rec e ^ ")"
	| Case(e1, e2, e3) ->
		let x = make_new_var () in
		"((" ^ x ^ " => (if " ^ x ^ "->0; " ^ compile_rec e3 ^ "; else "
			^ compile_rec e2 ^ "; end)(" ^ x ^ "->1)) " ^ compile_rec e1 ^ ")"
	| Allocation e -> "{value: " ^ compile_rec e ^ "}"
	| Assignment(e1, e2) -> "(" ^ compile_rec e1 ^ "->'value' = " ^ compile_rec e2 ^ ")"
	| Dereference e -> "(" ^ compile_rec e ^ "->'value')"
	| Sequence(e1, e2) -> "((func: ; " ^ compile_rec e1 ^ "; " ^ compile_rec e2 ^ "; end)())"
	| Record lst ->
		let foldf rest (l, e) = "\"" ^ l ^ "\": " ^ compile_rec e ^ ", " ^ rest in
		"{" ^ List.fold_left foldf "" lst ^ "}"
	| Member(e, l) -> "((" ^ compile_rec e ^ ")->'" ^ l ^ "')"
	| Let(x, t, e1, e2) -> compile_rec(Application(Abstraction(x, t, e2), e1))
	| LetRec(x, t, e1, e2) -> compile_rec(Application(Abstraction(x, t, e2), Fix(Abstraction(x, t, e1))))

let compile_eh e = "echo ('Result: ' + " ^ compile_rec e ^ ");"
