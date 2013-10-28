open Ast

let z =
	let inner_z = Abstraction("x", None, Application(Var "f", Abstraction("y", None, Application(Application(Var "x", Var "x"), Var "y")))) in
	Abstraction("f", None, Application(inner_z, inner_z))

let translate_var v =
	"v" ^ Str.global_replace (Str.regexp "'") "_u" v

let create_new_var : unit -> string =
	let n = ref 0 in
	fun () ->
		let curr = !n in
		n := curr + 1;
		"v" ^ string_of_int curr ^ "_c"

type desugar_ctxt = expr VarMap.t

let rec desugar (e : expr) (vm : desugar_ctxt) : expr = match e with
	| Var _ | Int _ | Bool _ | Unit | Error _ | String _ -> e
	| Application(e1, e2) -> Application(desugar e1 vm, desugar e2 vm)
	| Abstraction(arg, t, body) -> Abstraction(arg, t, desugar body vm)
	| Binop(op, e1, e2) -> Binop(op, desugar e1 vm, desugar e2 vm)
	| Boolbinop(op, e1, e2) -> Boolbinop(op, desugar e1 vm, desugar e2 vm)
	| Fix(e) -> Fix(desugar e vm)
	| If(e1, e2, e3) -> If(desugar e1 vm, desugar e2 vm, desugar e3 vm)
	| Pair(e1, e2) -> Pair(desugar e1 vm, desugar e2 vm)
	| Projection(b, e) -> Projection(b, desugar e vm)
	| Injection(b, e) -> Injection(b, desugar e vm)
	| Case(e1, e2, e3) -> Case(desugar e1 vm, desugar e2 vm, desugar e3 vm)
	| Allocation e -> Allocation(desugar e vm)
	| Assignment(e1, e2) -> Assignment(desugar e1 vm, desugar e2 vm)
	| Dereference e -> Dereference(desugar e vm)
	| Sequence(e1, e2) -> Sequence(desugar e1 vm, desugar e2 vm)
	| Record lst -> Record(VarMap.map (fun e -> desugar e vm) lst)
	| Member(e, l) -> Member(desugar e vm, l)
	| In(Let(x, t, e1), e2) -> Application(Abstraction(x, t, desugar e2 vm), desugar e1 vm)
	| In(LetRec(x, t, e1), e2) -> Application(Abstraction(x, t, desugar e2 vm), Fix(Abstraction(x, t, desugar e1 vm)))
	| In(TypeSynonym(_, _), e) -> desugar e vm
	| In(LetADT(_, _, lst), e) ->
		let _, vm' = desugar_adt lst vm in
		desugar e vm'
	| In(SingleExpression e1, e2) -> Sequence(desugar e1 vm, desugar e2 vm)
	| Constructor x -> VarMap.find x vm
	| Dummy _ -> failwith "Should not appear in code"
	| Match(e, lst) ->
		let x = create_new_var() in
		let foldf rest (p, e) =
			let rec compile_pattern p e cont = match p with
			| PInt n -> If(Boolbinop(Equals, Int n, e), Injection(true, cont), Injection(false, Unit))
			| PBool b -> If(Boolbinop(Equals, Bool b, e), Injection(true, cont), Injection(false, Unit))
			| PString s -> If(Boolbinop(Equals, String s, e), Injection(true, cont), Injection(false, Unit))
			| PAnything -> Injection(true, cont)
			| PVariable x -> Injection(true, In(Let(x, None, e), cont))
			| PApplication(p1, p2)
			| PPair(p1, p2) ->
				(* case match (fst e) c of \_. inl () | \x. match (snd e) x *)
				let fst = Projection(false, e) in
				let snd = Projection(true, e) in
				let p2' = compile_pattern p2 snd cont in
				let p1_failed = Abstraction("_", None, Injection(false, Unit)) in
				let p1_succeeded = Abstraction("x", None, Var "x") in
				Case(compile_pattern p1 fst p2', p1_failed, p1_succeeded)
			| PGuarded(p, e') ->
				let cont' = If(desugar e' vm, Injection(true, cont), Injection(false, Unit)) in
				let p_failed = Abstraction("_", None, Injection(false, Unit)) in
				let p_succeeded = Abstraction("x", None, Var "x") in
				Case(compile_pattern p e cont', p_failed, p_succeeded)
			| PAs(p, x) ->
				let cont' = In(Let(x, None, e), cont) in
				compile_pattern p e cont'
			| PConstructor x -> If(Boolbinop(Equals, Constructor x, e), Injection(true, cont), Injection(false, Unit))
			in
			let new_x = create_new_var() in
			let compiled_p = compile_pattern p (Var x) (Abstraction(new_x, None, desugar e vm)) in
			Case(compiled_p, Abstraction(new_x, None, rest), Abstraction(new_x, None, Application(Var new_x, Unit)))
		in
		let mtch = List.fold_left foldf (Error "Inexhaustive pattern match") (List.rev lst) in
		Application(Abstraction(x, None, mtch), desugar e vm)
	| ConstructorMember(e, l) -> Member(desugar e vm, l)
	| In(Open m, e) -> In(Open m, desugar e vm)
	| In(Import m, e) -> In(Import m, desugar e vm)
	| Module(t, lst) ->
		let rec loop lst vm = match lst with
			| Let(x, t, e)::tl -> Let(x, t, desugar e vm)::loop tl vm
			| LetRec(x, t, e)::tl -> LetRec(x, t, desugar e vm)::loop tl vm
			| Open s::tl -> Open s::loop tl vm
			| Import s::tl -> Import s::loop tl vm
			| SingleExpression e::tl -> SingleExpression (desugar e vm)::loop tl vm
			| TypeSynonym _::tl -> loop tl vm
			| LetADT(_, _, lst)::tl ->
				let vars, vm' = desugar_adt lst vm in
				vars @ loop tl vm'
			| [] -> []
		in
		Module(t, loop lst vm)
and desugar_adt lst vm =
	(* ADT constructors are converted into functions that return nested
		pairs of values. For example, Cons would become
				\hd. \tl. [["Cons", hd], tl])
		That means we can simply treat ADT construction as a function call.
		Pattern matching can treat ADT deconstruction the same way as pair
		deconstruction. *)
	let foldf (lets, rest) (name, lst) =
		let vars = List.mapi (fun i _ -> "x" ^ string_of_int i) lst in
		let result = List.fold_left (fun rest v -> Pair(rest, Var v)) (Constructor name) vars in
		let f = List.fold_right (fun v rest -> Abstraction(v, None, rest)) vars result in
		(Let(name, None, f)::lets, VarMap.add name f rest)
	in
	List.fold_left foldf ([], vm) lst

let desugar e = desugar e VarMap.empty

let rec compile_rec e = match e with
	| Var x -> translate_var x
	| Application(e1, e2) -> "(" ^ compile_rec e1 ^ "(" ^ compile_rec e2 ^ "))"
	| Abstraction(arg, _, body) -> "(function(" ^ translate_var arg ^ ") {return (" ^ compile_rec body ^ ");})"
	| Int n -> string_of_int n
	| String s -> "\"" ^ s ^ "\""
	| Binop(op, e1, e2) -> "(" ^ compile_rec e1 ^ string_of_binop op ^ compile_rec e2 ^ ")"
	| Boolbinop(Equals, e1, e2) -> "(" ^ compile_rec e1 ^ " == " ^ compile_rec e2 ^ ")"
	| Boolbinop(op, e1, e2) -> "(" ^ compile_rec e1 ^ string_of_bool_binop op ^ compile_rec e2 ^ ")"
(* 	| Unop(Print, e) -> "((function(x) {console.log(x);return x;})(" ^ compile_rec e ^ "))" *)
	| Fix(Abstraction(arg, t, body)) ->
		(* Apply the Z combinator *)
		compile_rec(Application(z, Abstraction(arg, t, body)))
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
		let foldf l e rest = "\"" ^ l ^ "\": " ^ compile_rec e ^ ", " ^ rest in
		"{" ^ VarMap.fold foldf lst "" ^ "}"
	| Member(e, l) -> "(" ^ compile_rec e ^ ")." ^ translate_var l
	| In(Let(x, t, e1), e2) -> compile_rec(Application(Abstraction(x, t, e2), e1))
	| In(LetRec(x, t, e1), e2) -> compile_rec(Application(Abstraction(x, t, e2), Fix(Abstraction(x, t, e1))))
	| Error e -> "((function() { throw new Error(\"" ^ e ^ "\"); })())"
	| Constructor x -> "\"" ^ x ^ "\""
	| Module(_, lst) ->
		let foldf e (body, result) = match e with
		| SingleExpression e -> compile_rec e ^ "; " ^ body, result
		| Let(x, _, e) | LetRec(x, _, e) ->
			let body' = "var " ^ translate_var x ^ " = " ^ compile_rec e ^ "; " ^ body in
			let result' = "\"" ^ translate_var x ^ "\": " ^ translate_var x ^ ", " ^ result in
			body', result'
		| Open s | Import s -> failwith "Not implemented"
		| LetADT(_, _, _) | TypeSynonym(_, _) -> failwith "Impossible"
		in
		let body, result = List.fold_right foldf lst ("", "") in
		"(function() { " ^ body ^ " return {" ^ result ^ "};})()"
	| ConstructorMember(_, _)
	| Dummy _ | In _ | Fix _ | Match(_, _) -> failwith "Impossible"

let compile e =
	let e' = desugar e in
	"console.log(\"Result: \" + " ^ compile_rec e' ^ ");"

let rec compile_rec e = match e with
	| Var x -> translate_var x
	| Application(e1, e2) -> "(" ^ compile_rec e1 ^ " " ^ compile_rec e2 ^ ")"
	| Abstraction(arg, _, body) -> "(fun " ^ translate_var arg ^ " -> " ^ compile_rec body ^ ")"
	| Int n -> string_of_int n
	| String s -> "\"" ^ s ^ "\""
	| Bool true -> "true"
	| Bool false -> "false"
	| Unit -> "()"
	| Binop(op, e1, e2) -> "(" ^ compile_rec e1 ^ string_of_binop op ^ compile_rec e2 ^ ")"
	| Boolbinop(op, e1, e2) -> "(" ^ compile_rec e1 ^ string_of_bool_binop op ^ compile_rec e2 ^ ")"
(* 	| Unop(Print, e) -> "(let e = " ^ compile_rec e ^ " in Printf.printf \"%d\\n\" e; e)" *)
	| If(e1, e2, e3) -> "(if " ^ compile_rec e1 ^ " then " ^ compile_rec e2 ^ " else " ^ compile_rec e3 ^ ")"
	| Fix(Abstraction(arg, t, body)) -> "(let rec " ^ translate_var arg ^ " = " ^ compile_rec body ^ " in " ^ translate_var arg ^ ")"
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
	| In(Let(x, t, e1), e2) -> compile_rec(Application(Abstraction(x, t, e2), e1))
	| In(LetRec(x, t, e1), e2) -> compile_rec(Application(Abstraction(x, t, e2), Fix(Abstraction(x, t, e1))))
	| Error e -> "(failwith \"" ^ e ^ "\")"
	| Constructor x -> "\"" ^ x ^ "\""
	| ConstructorMember(e, l) -> failwith "not implemented"
	| Module _ -> failwith "Not implemented"
	| Dummy _ | In _ | Match(_, _)
	| Fix _ -> failwith "Impossible"

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
	| String s -> "\"" ^ s ^ "\""
	| Bool true -> "true"
	| Bool false -> "false"
	| Unit -> "()"
	| Binop(op, e1, e2) -> "(" ^ compile_rec e1 ^ " " ^ string_of_binop op ^ " " ^ compile_rec e2 ^ ")"
	| Boolbinop(Equals, e1, e2) -> "(" ^ compile_rec e1 ^ " == " ^ compile_rec e2 ^ ")"
	| Boolbinop(op, e1, e2) -> "(" ^ compile_rec e1 ^ " " ^ string_of_bool_binop op ^ " " ^ compile_rec e2 ^ ")"
(* 	| Unop(Print, e) -> "((func: x; echo x; x; end)" ^ compile_rec e ^ ")" *)
	| If(e1, e2, e3) -> "(if (" ^ compile_rec e1 ^ "); " ^ compile_rec e2 ^ "; else " ^ compile_rec e3 ^ "; end)"
	| Fix(Abstraction(arg, _, body)) ->
		"((func: ; private " ^ translate_var arg ^ " = " ^ compile_rec body ^ "; "
			^ translate_var arg ^ "; end) ())"
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
		let foldf l e rest = "\"" ^ l ^ "\": " ^ compile_rec e ^ ", " ^ rest in
		"{" ^ VarMap.fold foldf lst "" ^ "}"
	| Member(e, l) -> "((" ^ compile_rec e ^ ")->'" ^ l ^ "')"
	| In(Let(x, t, e1), e2) -> compile_rec(Application(Abstraction(x, t, e2), e1))
	| In(LetRec(x, t, e1), e2) -> compile_rec(Application(Abstraction(x, t, e2), Fix(Abstraction(x, t, e1))))
	| Error e -> "(throw(Exception.new(\"" ^ e ^ "\")))"
	| Constructor x -> "\"" ^ x ^ "\""
	| ConstructorMember(_, _)
	| Module _ -> failwith "Not implemented"
	| In _ | Match(_, _) | Fix _ | Dummy _ -> failwith "Impossible"

let compile_eh e =
	let e' = desugar e in
	"echo 'Result: '; printvar (" ^ compile_rec e' ^ ");"
