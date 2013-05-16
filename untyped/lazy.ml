open Ast

exception RuntimeError of string

(* Values specialized for lazy evaluation *)
type lvalue =
	LClosure of string * lazy_value ref VarMap.t * expr
	| LInteger of int
and lazy_value = Unevaluated of lazy_value ref VarMap.t * expr | Evaluated of lvalue

let string_of_lvalue v =
	match v with
	| LClosure(x, _, body) -> "\\" ^ x ^ ". " ^ string_of_expr body
	| LInteger n -> string_of_int n

let rec eval (e : expr) (env : lazy_value ref VarMap.t) =
	match e with
	| Integer n -> LInteger n
	| Var x -> (try let v = VarMap.find x env in
		match !v with
		| Unevaluated(env', e) ->
			let v' = eval e env' in
			v := Evaluated v';
			v'
		| Evaluated v -> v
		with Not_found -> raise(RuntimeError("Unbound variable: " ^ x)))
	| Binop(op, e1, e2) ->
		(* These operators are strict in their arguments. *)
		let e1' = eval e1 env in
		let e2' = eval e2 env in
		(match e1', e2' with
		| LInteger n1, LInteger n2 -> LInteger(f_of_binop op n1 n2)
		| LInteger _, _ -> failwith("Invalid operand to binary expression: " ^ string_of_lvalue e2')
		| _, _ -> failwith("Invalid operand to binary expression: " ^ string_of_lvalue e1'))
	| Unop(op, e) ->
		let e' = eval e env in
		(match e' with
		| LInteger n -> LInteger(f_of_unop op n)
		| _ -> failwith("Invalid operand to unary expression: " ^ string_of_expr e))
	| Abstraction(arg, body) -> LClosure(arg, env, body)
	| Application(e1, e2) -> (match eval e1 env with
		| LClosure(arg, env', body) ->
			let env'' = VarMap.add arg (ref (Unevaluated(env, e2))) env' in
			eval body env''
		| _ -> raise(RuntimeError("This expression is not a function; it cannot be applied")))

let rec eval_lazy (e : expr) : value =
	match eval e VarMap.empty with
	| LClosure(arg, _, body) -> VClosure(arg, VarMap.empty, body)
	| LInteger n -> VInteger n
