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

let rec substitute code var replacement = match code with
	| Var(x) -> if x = var then replacement else Var x
	| Application(e1, e2) -> Application(substitute e1 var replacement, substitute e2 var replacement)
	| Abstraction(arg, body) ->
		if arg = var then Abstraction(arg, body)
		else if not (is_free_variable arg replacement)
			then Abstraction(arg, substitute body var replacement)
			else let var_name = get_unique_var_name () in
				Abstraction(var_name, substitute (substitute body arg (Var var_name)) var replacement)

let rec eval e = match e with
	| Var x -> failwith ("Unbound variable: " ^ x)
	| Abstraction(_, _) -> e
	| Application(e1, e2) ->
		let e1' = eval e1 in
		let e2' = eval e2 in
		match e1' with
		| Abstraction(arg, body) ->
			let substituted = substitute body arg e2' in
			eval substituted
		| _ -> failwith "This expression is not a function; it cannot be applied"
