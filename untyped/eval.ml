open Ast

type semantics = CBV | CBN

let rec eval (e : expr) (env : value VarMap.t) (s : semantics) : value = match e with
	| Var x -> (try match VarMap.find x env with
		| VDummy(env', e') -> eval e' env' s
		| v -> v
		with Not_found -> failwith("Unbound variable: " ^ x))
	| Abstraction(x, body) -> VClosure(x, env, body)
	| Integer n -> VInteger n
	| String s -> VString s
	| Binop(op, e1, e2) ->
		let e1' = eval e1 env s in
		let e2' = eval e2 env s in
		(match e1', e2' with
		| VInteger n1, VInteger n2 -> VInteger(f_of_binop op n1 n2)
		| VInteger _, _ -> failwith("Invalid operand to binary expression: " ^ string_of_value e2')
		| _, _ -> failwith("Invalid operand to binary expression: " ^ string_of_value e1'))
	| Unop(op, e) ->
		let e' = eval e env s in
		(match e' with
		| VInteger n -> VInteger(f_of_unop op n)
		| _ -> failwith("Invalid operand to unary expression: " ^ string_of_expr e))
	| Application(e1, e2) ->
		let e1' = eval e1 env s in
		let e2' = match s with
			| CBV -> eval e2 env s
			| CBN -> VDummy(env, e2) in
		match e1' with
		| VClosure(arg, env', body) ->
			eval body (VarMap.add arg e2' env') s
		| _ -> failwith "This expression is not a function; it cannot be applied"

let eval_cbv e = eval e VarMap.empty CBV
let eval_cbn e = eval e VarMap.empty CBN
