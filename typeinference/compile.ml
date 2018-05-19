open Ast

let dummy_t = ref None

let z =
	let inner_z = Abstraction("x", dummy_t,
		(Application((Var "f", dummy_t),
			(Abstraction("y", dummy_t,
				(Application(
					(Application((Var "x", dummy_t), (Var "x", dummy_t)), dummy_t),
					(Var "y", dummy_t)),
				dummy_t)),
			dummy_t)),
		dummy_t)), dummy_t in
	(Abstraction("f", dummy_t, (Application(inner_z, inner_z), dummy_t)), dummy_t)

let translate_var v =
	"v" ^ Str.global_replace (Str.regexp "'") "_u" v

let create_new_var : unit -> string =
	let n = ref 0 in
	fun () ->
		let curr = !n in
		n := curr + 1;
		"v" ^ string_of_int curr ^ "_c"

type desugar_ctxt = expr VarMap.t

let rec desugar (e, t : typed_expr) (vm : desugar_ctxt) : typed_expr = (match e with
	| Var _ | Int _ | Bool _ | Unit | Error _ | String _ -> e
	| Wrapped e -> let e, _ = desugar e vm in e
	| Application(e1, e2) -> Application(desugar e1 vm, desugar e2 vm)
	| Abstraction(arg, t, body) -> Abstraction(arg, t, desugar body vm)
	| Binop(op, e1, e2) -> Binop(op, desugar e1 vm, desugar e2 vm)
	| Boolbinop(op, e1, e2) -> Boolbinop(op, desugar e1 vm, desugar e2 vm)
	| Fix(e) -> Fix(desugar e vm)
	| If(e1, e2, e3) -> If(desugar e1 vm, desugar e2 vm, desugar e3 vm)
	| Pair(e1, e2) -> Pair(desugar e1 vm, desugar e2 vm)
	| Projection(b, e) -> Projection(b, desugar e vm)
	| Injection(b, e) -> Injection(b, desugar e vm)
	| RevealType(e) -> RevealType(desugar e vm)
	| Case(e1, e2, e3) -> Case(desugar e1 vm, desugar e2 vm, desugar e3 vm)
	| Allocation e -> Allocation(desugar e vm)
	| Assignment(e1, e2) -> Assignment(desugar e1 vm, desugar e2 vm)
	| Dereference e -> Dereference(desugar e vm)
	| Sequence(e1, e2) -> Sequence(desugar e1 vm, desugar e2 vm)
	| Record lst -> Record(VarMap.map (fun e -> desugar e vm) lst)
	| Member(e, l) -> Member(desugar e vm, l)
	| In(Let(x, t, e1), e2) ->
		Application((Abstraction(x, t, desugar e2 vm), dummy_t), desugar e1 vm)
	| In(LetRec(x, t, e1), e2) ->
		Application((Abstraction(x, t, desugar e2 vm), dummy_t), (Fix(Abstraction(x, t, desugar e1 vm), dummy_t), dummy_t))
	| In(TypeSynonym(_, _), e) -> Wrapped(desugar e vm)
	| In(LetADT(_, _, lst), e) ->
		let _, vm' = desugar_adt lst vm in
		Wrapped(desugar e vm')
	| In(SingleExpression e1, e2) -> Sequence(desugar e1 vm, desugar e2 vm)
	| Constructor x -> VarMap.find x vm
	| Dummy _ -> failwith "Should not appear in code"
	| Match(e, lst) ->
		let x = create_new_var() in
		let inj b c = Injection(b, (c, dummy_t)), dummy_t in
		let id = Abstraction("x", dummy_t, (Var "x", dummy_t)), dummy_t in
		let foldf rest (p, e) =
			let rec compile_pattern p e cont = match p with
			| PInt n -> If((Boolbinop(Equals, (Int n, dummy_t), e), dummy_t), inj true cont, inj false Unit)
			| PBool b -> If((Boolbinop(Equals, (Bool b, dummy_t), e), dummy_t), inj true cont, inj false Unit)
			| PString s -> If((Boolbinop(Equals, (String s, dummy_t), e), dummy_t), inj true cont, inj false Unit)
			| PAnything -> Injection(true, (cont, dummy_t))
			| PVariable x -> Injection(true, (In(Let(x, dummy_t, e), (cont, dummy_t)), dummy_t))
			| PApplication(p1, p2)
			| PPair(p1, p2) ->
				(* case match (fst e) c of \_. inl () | \x. match (snd e) x *)
				let fst = Projection(false, e), dummy_t in
				let snd = Projection(true, e), dummy_t in
				let p2' = compile_pattern p2 snd cont in
				let p1_failed = Abstraction("_", dummy_t, inj false Unit), dummy_t in
				Case((compile_pattern p1 fst p2', dummy_t), p1_failed, id)
			| PGuarded(p, e') ->
				let cont' = If(desugar e' vm, inj true cont, inj false Unit) in
				let p_failed = Abstraction("_", dummy_t, inj false Unit), dummy_t in
				Case((compile_pattern p e cont', dummy_t), p_failed, id)
			| PAs(p, x) ->
				let cont' = In(Let(x, dummy_t, e), (cont, dummy_t)) in
				compile_pattern p e cont'
			| PConstructor x ->
				If((Boolbinop(Equals, (Constructor x, dummy_t), e), dummy_t), inj true cont, inj false Unit)
			in
			let new_x = create_new_var() in
			let compiled_p = compile_pattern p (Var x, dummy_t) (Abstraction(new_x, dummy_t, desugar e vm)), dummy_t in
			let lcase = Abstraction(new_x, dummy_t, rest), dummy_t in
			let rcase = Abstraction(new_x, dummy_t, (Application((Var new_x, dummy_t), (Unit, dummy_t)), dummy_t)), dummy_t in
			Case(compiled_p, lcase, rcase), dummy_t
		in
		let mtch = List.fold_left foldf (Error "Inexhaustive pattern match", dummy_t) (List.rev lst) in
		Application((Abstraction(x, dummy_t, mtch), dummy_t), desugar e vm)
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
		Module(t, loop lst vm)), t
and desugar_adt lst vm =
	(* ADT constructors are converted into functions that return nested
		pairs of values. For example, Cons would become
				\hd. \tl. [["Cons", hd], tl])
		That means we can simply treat ADT construction as a function call.
		Pattern matching can treat ADT deconstruction the same way as pair
		deconstruction. *)
	let foldf (lets, rest) (name, lst) =
		let vars = List.mapi (fun i _ -> "x" ^ string_of_int i) lst in
		let result = List.fold_left (fun rest v -> Pair(rest, (Var v, dummy_t)), dummy_t) (Constructor name, dummy_t) vars in
		let f, t = List.fold_right (fun v rest -> Abstraction(v, dummy_t, rest), dummy_t) vars result in
		(Let(name, dummy_t, (f, t))::lets, VarMap.add name f rest)
	in
	List.fold_left foldf ([], vm) lst

let desugar e = desugar e VarMap.empty

let rec compile_rec (e, _) = match e with
	| Var x -> translate_var x
	| Wrapped e -> compile_rec e
	| Application(e1, e2) -> "(" ^ compile_rec e1 ^ "(" ^ compile_rec e2 ^ "))"
	| Abstraction(arg, _, body) -> "(function(" ^ translate_var arg ^ ") {return (" ^ compile_rec body ^ ");})"
	| Int n -> string_of_int n
	| String s -> "\"" ^ s ^ "\""
	| Binop(op, e1, e2) -> "(" ^ compile_rec e1 ^ string_of_binop op ^ compile_rec e2 ^ ")"
	| Boolbinop(Equals, e1, e2) -> "(" ^ compile_rec e1 ^ " == " ^ compile_rec e2 ^ ")"
	| Boolbinop(op, e1, e2) -> "(" ^ compile_rec e1 ^ string_of_bool_binop op ^ compile_rec e2 ^ ")"
(* 	| Unop(Print, e) -> "((function(x) {console.log(x);return x;})(" ^ compile_rec e ^ "))" *)
	| Fix(Abstraction(arg, t, body), _) ->
		(* Apply the Z combinator *)
		compile_rec(Application(z, (Abstraction(arg, t, body), dummy_t)), dummy_t)
	| If(e1, e2, e3) -> "((" ^ compile_rec e1 ^ ") ? (" ^ compile_rec e2 ^ ") : (" ^ compile_rec e3 ^ "))"
	| Bool true -> "true"
	| Bool false -> "false"
	| Pair(e1, e2) -> "[" ^ compile_rec e1 ^ ", " ^ compile_rec e2 ^ "]"
	| Projection(false, e) -> "(" ^ compile_rec e ^ "[0])"
	| Projection(true, e) -> "(" ^ compile_rec e ^ "[1])"
	| RevealType(e) -> "(function(e) { console.log(typeof e); return e; }(" ^ compile_rec e ^ ")"
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
	| In(Let(x, t, e1), e2) ->
		compile_rec(Application((Abstraction(x, t, e2), dummy_t), e1), dummy_t)
	| In(LetRec(x, t, e1), e2) ->
		let fix = Fix((Abstraction(x, t, e1), dummy_t)), dummy_t in
		compile_rec(Application((Abstraction(x, t, e2), dummy_t), fix), dummy_t)
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

let rec compile_rec (e, _) = match e with
	| Var x -> translate_var x
	| Wrapped e -> compile_rec e
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
	| Fix(Abstraction(arg, t, body), _) -> "(let rec " ^ translate_var arg ^ " = " ^ compile_rec body ^ " in " ^ translate_var arg ^ ")"
	| Pair(e1, e2) -> "(" ^ compile_rec e1 ^ ", " ^ compile_rec e2 ^ ")"
	| Projection(false, e) -> "(fst " ^ compile_rec e ^ ")"
	| Projection(true, e) -> "(snd " ^ compile_rec e ^ ")"
	| Injection(b, e) -> failwith "Not implemented"
	| Case(e1, e2, e3) -> failwith "Not implemented"
	| RevealType(e) -> failwith "Not implemented"
	| Allocation e -> "(ref " ^ compile_rec e ^ ")"
	| Assignment(e1, e2) -> "(" ^ compile_rec e1 ^ " := " ^ compile_rec e2 ^ ")"
	| Dereference e -> "(!" ^ compile_rec e ^ ")"
	| Sequence(e1, e2) -> "(ignore(" ^ compile_rec e1 ^ "); " ^ compile_rec e2 ^ ")"
	| Record lst -> failwith "Not implemented"
	| Member(e, l) -> failwith "Not implemented"
	| In(Let(x, t, e1), e2) ->
		compile_rec(Application((Abstraction(x, t, e2), dummy_t), e1), dummy_t)
	| In(LetRec(x, t, e1), e2) ->
		let fix = Fix((Abstraction(x, t, e1), dummy_t)), dummy_t in
		compile_rec(Application((Abstraction(x, t, e2), dummy_t), fix), dummy_t)
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

let rec compile_rec (e, _) = match e with
	| Var x -> translate_var x
	| Wrapped e -> compile_rec e
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
	| Fix(Abstraction(arg, _, body), _) ->
		"((func: ; private " ^ translate_var arg ^ " = " ^ compile_rec body ^ "; "
			^ translate_var arg ^ "; end) ())"
	| Pair(e1, e2) -> "(" ^ compile_rec e1 ^ ", " ^ compile_rec e2 ^ ")"
	| Projection(false, e) -> "(" ^ compile_rec e ^ "->0)"
	| Projection(true, e) -> "(" ^ compile_rec e ^ "->1)"
	| Injection(b, e) -> "(" ^ (if b then "true" else "false") ^ ", " ^ compile_rec e ^ ")"
	| RevealType(e) -> failwith "Not implemented"
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
	| In(Let(x, t, e1), e2) ->
		compile_rec(Application((Abstraction(x, t, e2), dummy_t), e1), dummy_t)
	| In(LetRec(x, t, e1), e2) ->
		let fix = Fix((Abstraction(x, t, e1), dummy_t)), dummy_t in
		compile_rec(Application((Abstraction(x, t, e2), dummy_t), fix), dummy_t)
	| Error e -> "(throw(Exception.new(\"" ^ e ^ "\")))"
	| Constructor x -> "\"" ^ x ^ "\""
	| ConstructorMember(_, _)
	| Module _ -> failwith "Not implemented"
	| In _ | Match(_, _) | Fix _ | Dummy _ -> failwith "Impossible"

let compile_eh e =
	let e' = desugar e in
	"echo 'Result: '; printvar (" ^ compile_rec e' ^ ");"
