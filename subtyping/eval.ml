open Ast

type semantics = CBV | CBN

let get_unique_var_name : unit -> string =
	let current = ref 0 in
	fun () ->
		let index = !current in
		current := index + 1;
		"'" ^ (string_of_int index)

let rec is_free_variable (var : string) (code : expr) : bool = match code with
	| Var x -> var = x
	| Application(e1, e2)
	| Pair(e1, e2) -> is_free_variable var e1 || is_free_variable var e2
	| Abstraction(arg, _, body) -> (arg <> var) && (is_free_variable var body)
	| Int _ | Bool _ | Reference _ | Unit -> false
	| Assignment(e1, e2)
	| Sequence(e1, e2)
	| Binop(_, e1, e2)
	| Boolbinop(_, e1, e2) -> is_free_variable var e1 || is_free_variable var e2
	| Case(e1, e2, e3)
	| If(e1, e2, e3) -> is_free_variable var e1 || is_free_variable var e2 || is_free_variable var e3
	| Allocation e
	| Dereference e
	| Unop(_, e)
	| Member(e, _)
	| Fix e -> is_free_variable var e
	| Projection(_, e)
	| Injection(_, e) -> is_free_variable var e
	| Record lst -> List.exists (fun (l, e) -> is_free_variable var e) lst
	| Let(x, t, e1, e2) -> is_free_variable var e1 || (x <> var && is_free_variable var e2)
	| LetRec(x, t, e1, e2) -> (x <> var) && (is_free_variable var e1 || is_free_variable var e2)

let rec substitute (code : expr) (var : string) (replacement : expr) : expr = match code with
	| Var(x) -> if x = var then replacement else Var x
	| Application(e1, e2) -> Application(substitute e1 var replacement, substitute e2 var replacement)
	| Int _ | Bool _ | Reference _ | Unit -> code
	| Binop(op, e1, e2) -> Binop(op, substitute e1 var replacement, substitute e2 var replacement)
	| Boolbinop(op, e1, e2) -> Boolbinop(op, substitute e1 var replacement, substitute e2 var replacement)
	| If(e1, e2, e3) -> If(substitute e1 var replacement, substitute e2 var replacement, substitute e3 var replacement)
	| Unop(op, e) -> Unop(op, substitute e var replacement)
	| Fix e -> Fix(substitute e var replacement)
	| Allocation e -> Allocation(substitute e var replacement)
	| Assignment(e1, e2) -> Assignment(substitute e1 var replacement, substitute e2 var replacement)
	| Dereference e -> Dereference(substitute e var replacement)
	| Abstraction(arg, t, body) ->
		if arg = var then Abstraction(arg, t, body)
		else if not (is_free_variable arg replacement)
			then Abstraction(arg, t, substitute body var replacement)
			else let var_name = get_unique_var_name () in
				Abstraction(var_name, t, substitute (substitute body arg (Var var_name)) var replacement)
	| Pair(e1, e2) -> Pair(substitute e1 var replacement, substitute e2 var replacement)
	| Projection(b, e) -> Projection(b, substitute e var replacement)
	| Injection(b, e) -> Injection(b, substitute e var replacement)
	| Case(e1, e2, e3) -> Case(substitute e1 var replacement, substitute e2 var replacement, substitute e3 var replacement)
	| Sequence(e1, e2) -> Sequence(substitute e1 var replacement, substitute e2 var replacement)
	| Record lst -> Record(List.map (fun (l, e) -> l, substitute e var replacement) lst)
	| Member(e, l) -> Member(substitute e var replacement, l)
	| Let(x, t, e1, e2) when x = var -> Let(x, t, substitute e1 var replacement, e2)
	| Let(x, t, e1, e2) -> Let(x, t, substitute e1 var replacement, substitute e2 var replacement)
	| LetRec(x, t, e1, e2) when x = var -> code
	| LetRec(x, t, e1, e2) -> LetRec(x, t, substitute e1 var replacement, substitute e2 var replacement)

let rec eval (e : expr) (s : semantics) : expr = match e with
	| Var x -> failwith ("Unbound variable: " ^ x)
	| Abstraction(_, _, _) | Reference _ -> e
	| Int n -> Int n
	| Bool b -> Bool b
	| Unit -> Unit
	| Binop(op, e1, e2) ->
		let e1' = eval e1 s in
		let e2' = eval e2 s in
		(match e1', e2' with
		| Int n1, Int n2 -> Int(f_of_binop op n1 n2)
		| Int _, _ -> failwith("Invalid operand to binary expression: " ^ string_of_expr e2')
		| _, _ -> failwith("Invalid operand to binary expression: " ^ string_of_expr e1'))
	| Boolbinop(op, e1, e2) ->
		let e1' = eval e1 s in
		let e2' = eval e2 s in
		(match e1', e2' with
		| Int n1, Int n2 -> Bool(f_of_bool_binop op n1 n2)
		| Int _, _ -> failwith("Invalid operand to binary expression: " ^ string_of_expr e2')
		| _, _ -> failwith("Invalid operand to binary expression: " ^ string_of_expr e1'))
	| Unop(op, e) ->
		let e' = eval e s in
		(match e' with
		| Int n -> Int(f_of_unop op n)
		| _ -> failwith("Invalid operand to unary expression: " ^ string_of_expr e))
	| Application(e1, e2) ->
		let e1' = eval e1 s in
		let e2' = match s with
			| CBV -> eval e2 s
			| CBN -> e2 in
		(match e1' with
		| Abstraction(arg, _, body) ->
			let substituted = substitute body arg e2' in
			eval substituted s
		| _ -> failwith "This expression is not a function; it cannot be applied")
	| Fix(Abstraction(arg, _, body)) ->
		eval (substitute body arg e) s
	| Fix _ -> failwith "Invalid use of fix"
	| If(e1, e2, e3) -> (match eval e1 s with
		| Bool true -> eval e2 s
		| Bool false -> eval e3 s
		| _ -> failwith "If block must contain a bool")
	| Pair(e1, e2) ->
		let e1' = eval e1 s in
		let e2' = eval e2 s in
		Pair(e1', e2')
	| Projection(b, e) -> (match eval e s with
		| Pair(e1, e2) -> if b then e2 else e1
		| _ -> failwith "This expression is not a product; it cannot be projected")
	| Injection(b, e) -> Injection(b, eval e s)
	| Case(e1, e2, e3) -> (match eval e1 s with
		| Injection(false, e') -> eval (Application(e2, e')) s
		| Injection(true, e') -> eval (Application(e3, e')) s
		| _ -> failwith "This expression is not a sum; it cannot be matched on")
	| Assignment(e1, e2) -> (match eval e1 s with
		| Reference r -> r := (eval e2 s)
		| _ -> failwith "This expression is not a reference; it cannot be assigned to"); Unit
	| Allocation e -> Reference(ref (eval e s))
	| Dereference e -> (match eval e s with
		| Reference r -> !r
		| _ -> failwith "This expression is not a reference; it cannot be dereferenced")
	| Sequence(e1, e2) -> let _ = eval e1 s in eval e2 s
	| Record lst -> Record(List.map (fun (l, e) -> l, eval e s) lst)
	| Member(e, l) -> (match eval e s with
		| Record lst -> (try List.assoc l lst
			with Not_found -> failwith("Unknown label: " ^ l))
		| _ -> failwith "This expression is not a record; member access is not possible")
	| Let(x, t, e1, e2) ->
		let e1' = eval e1 s in
		let substituted = substitute e2 x e1' in
		eval substituted s
	| LetRec(x, t, e1, e2) ->
		let e1' = eval (Fix(Abstraction(x, t, e1))) s in
		let substituted = substitute e2 x e1' in
		eval substituted s

let eval_cbv e = eval e CBV
let eval_cbn e = eval e CBN
