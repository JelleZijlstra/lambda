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
	| Application(e1, e2) -> (is_free_variable var e1) || (is_free_variable var e2)
	| Abstraction(arg, _, body) -> (arg <> var) && (is_free_variable var body)
	| Integer _ | Boolean _ -> false
	| Binop(_, e1, e2) -> is_free_variable var e1 || is_free_variable var e2
	| Boolbinop(_, e1, e2) -> is_free_variable var e1 || is_free_variable var e2
	| If(e1, e2, e3) -> is_free_variable var e1 || is_free_variable var e2 || is_free_variable var e3
	| Unop(_, e) -> is_free_variable var e
	| Fix e -> is_free_variable var e

let rec substitute (code : expr) (var : string) (replacement : expr) : expr = match code with
	| Var(x) -> if x = var then replacement else Var x
	| Application(e1, e2) -> Application(substitute e1 var replacement, substitute e2 var replacement)
	| Integer _ | Boolean _ -> code
	| Binop(op, e1, e2) -> Binop(op, substitute e1 var replacement, substitute e2 var replacement)
	| Boolbinop(op, e1, e2) -> Boolbinop(op, substitute e1 var replacement, substitute e2 var replacement)
	| If(e1, e2, e3) -> If(substitute e1 var replacement, substitute e2 var replacement, substitute e3 var replacement)
	| Unop(op, e) -> Unop(op, substitute e var replacement)
	| Fix e -> Fix(substitute e var replacement)
	| Abstraction(arg, t, body) ->
		if arg = var then Abstraction(arg, t, body)
		else if not (is_free_variable arg replacement)
			then Abstraction(arg, t, substitute body var replacement)
			else let var_name = get_unique_var_name () in
				Abstraction(var_name, t, substitute (substitute body arg (Var var_name)) var replacement)


let rec eval (e : expr) (s : semantics) : expr = match e with
	| Var x -> failwith ("Unbound variable: " ^ x)
	| Abstraction(_, _, _) -> e
	| Integer n -> Integer n
	| Boolean b -> Boolean b
	| Binop(op, e1, e2) ->
		let e1' = eval e1 s in
		let e2' = eval e2 s in
		(match e1', e2' with
		| Integer n1, Integer n2 -> Integer(f_of_binop op n1 n2)
		| Integer _, _ -> failwith("Invalid operand to binary expression: " ^ string_of_expr e2')
		| _, _ -> failwith("Invalid operand to binary expression: " ^ string_of_expr e1'))
	| Boolbinop(op, e1, e2) ->
		let e1' = eval e1 s in
		let e2' = eval e2 s in
		(match e1', e2' with
		| Integer n1, Integer n2 -> Boolean(f_of_bool_binop op n1 n2)
		| Integer _, _ -> failwith("Invalid operand to binary expression: " ^ string_of_expr e2')
		| _, _ -> failwith("Invalid operand to binary expression: " ^ string_of_expr e1'))
	| Unop(op, e) ->
		let e' = eval e s in
		(match e' with
		| Integer n -> Integer(f_of_unop op n)
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
		| Boolean true -> eval e2 s
		| Boolean false -> eval e3 s
		| _ -> failwith "If block must contain a bool")

let eval_cbv e = eval e CBV
let eval_cbn e = eval e CBN
