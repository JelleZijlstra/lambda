open Ast

let get_unique_var_name =
	let current = ref 0 in
	fun () ->
		let index = !current in
		current := index + 1;
		"'" ^ (string_of_int index)

let rec is_free_variable var code = match code with
	| Var x -> var = x
	| Application(e1, e2) -> (is_free_variable var e1) || (is_free_variable var e2)
	| Abstraction(arg, body) -> (arg <> var) && (is_free_variable var body)
	| Integer _ -> false
	| Binop(_, e1, e2) -> is_free_variable var e1 || is_free_variable var e2

let rec substitute code var replacement = match code with
	| Var(x) -> if x = var then replacement else Var x
	| Application(e1, e2) -> Application(substitute e1 var replacement, substitute e2 var replacement)
	| Integer _ -> code
	| Binop(op, e1, e2) -> Binop(op, substitute e1 var replacement, substitute e1 var replacement)
	| Abstraction(arg, body) ->
		if arg = var then Abstraction(arg, body)
		else if not (is_free_variable arg replacement)
			then Abstraction(arg, substitute body var replacement)
			else let var_name = get_unique_var_name () in
				Abstraction(var_name, substitute (substitute body arg (Var var_name)) var replacement)

let rec eval e = match e with
	| Var x -> failwith ("Unbound variable: " ^ x)
	| Abstraction(_, _) -> e
	| Integer n -> Integer n
	| Binop(op, e1, e2) -> eval_binop op e1 e2
	| Application(e1, e2) ->
		let e1' = eval e1 in
		let e2' = eval e2 in
		match e1' with
		| Abstraction(arg, body) ->
			let substituted = substitute body arg e2' in
			eval substituted
		| _ -> failwith "This expression is not a function; it cannot be applied"
and eval_binop op e1 e2 =
	let e1' = eval e1 in
	let e2' = eval e2 in
	match e1', e2' with
	| Integer n1, Integer n2 -> Integer(f_of_binop op n1 n2)
	| Integer _, _ -> failwith("Invalid operand to binary expression: " ^ stringify e2')
	| _, _ -> failwith("Invalid operand to binary expression: " ^ stringify e1')
