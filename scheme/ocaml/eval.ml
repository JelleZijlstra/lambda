open Ast

exception RuntimeError of string

let rec set_args params args env = match params, args with
	| (Var name)::params', value::args' ->
		Environment.set env name value;
		set_args params' args' env
	| [Dotted (Var name)], lst -> Environment.set env name (List lst)
	| [], [] -> ()
	| (Var name)::_, [] -> raise (RuntimeError ("Argument " ^ name ^ " not given"))
	| _, _ -> raise (RuntimeError "Unable to set arguments")

let rec eval (e : expr) (env : environment) : expr =
	match e with
	| String _ | Integer _ | Bool _ | Closure _ | LibraryFunction _ | LibraryMacro _ -> e
	| Dotted _ -> raise (RuntimeError "Can't evaluate dotted expression")
	| Quoted e' -> e'
	| Var v -> (match Environment.get env v with
		| None -> raise (RuntimeError ("Unbound variable: " ^ v))
		| Some value -> value)
	| StatementList lst ->
		let rec eval_lst l env = match l with
		| [] -> raise (RuntimeError ("StatementList is empty"))
		| [hd] -> eval hd env
		| hd::tl -> let _ = eval hd env in eval_lst tl env
		in
		eval_lst lst env
	| List [] -> raise (RuntimeError "Can't evaluate empty list")
	| List(hd::tl) ->
		match eval hd env with
		| LibraryFunction lf -> lf (List.map (fun e -> eval e env) tl)
		| LibraryMacro lm -> lm tl env
		| Closure(params, env', body) ->
			let args = List.map (fun e -> eval e env) tl in
			let env'' = Environment.new_with_parent env' in
			set_args params args env'';
			eval body env''
		| _ -> raise (RuntimeError "This expression is not a function; it cannot be applied")
