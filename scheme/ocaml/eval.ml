open Ast

exception RuntimeError of string

let add_to_env key value env =
	{ names = (VarMap.add key value env.names); meval_context = env.meval_context }

let rec set_args params args env = match params, args with
	| (Var name)::params', value::args' ->
		let env' = add_to_env name value env in
		set_args params' args' env'
	| [Dotted (Var name)], lst -> add_to_env name (List lst) env
	| [], [] -> env
	| (Var name)::_, [] -> raise (RuntimeError ("Argument " ^ name ^ " not given"))
	| _, _ -> raise (RuntimeError "Unable to set arguments")

let rec eval_expr (e : expr) (env : environment) : expr * environment =
	match e with
	| String _ | Integer _ | Bool _ | Closure _ | LibraryFunction _ | LibraryMacro _ -> e, env
	| Dotted _ -> raise (RuntimeError "Can't evaluate dotted expression")
	| Quoted e' -> e', env
	| Var v ->
		(try
			VarMap.find v env.names, env
		with Not_found -> raise (RuntimeError ("Unbound variable: " ^ v)))
	| StatementList lst ->
		let rec eval_lst l env = match l with
		| [] -> raise (RuntimeError ("StatementList is empty"))
		| [hd] -> eval_expr hd env
		| hd::tl -> let _, env' = eval_expr hd env in eval_lst tl env'
		in
		eval_lst lst env
	| List [] -> raise (RuntimeError "Can't evaluate empty list")
	| List(hd::tl) ->
		let f, _ = eval_expr hd env in
		match f with
		| LibraryFunction lf ->
			let args = List.map (fun e -> let e', _ = eval_expr e env in e') tl in
			lf args, env
		| LibraryMacro lm -> lm tl env
		| Closure(params, env', body) ->
			let args = List.map (fun e -> let e', _ = eval_expr e env in e') tl in
			let env'' = set_args params args env' in
			let result, _ = eval_expr body env'' in
			result, env
		| _ -> raise (RuntimeError "This expression is not a function; it cannot be applied")




let rec eval (e : expr) (library : expr VarMap.t) : expr =
	let env = {names = library; meval_context = None} in
	let result, _ = eval_expr e env in
	result
