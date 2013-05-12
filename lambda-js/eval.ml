open Ast

exception RuntimeError of string
exception UserException of value
exception BreakException of string * value

module StringSet = Set.Make(struct
	type t = string
	let compare = compare
end)

let set_of_list lst = List.fold_left (fun x y -> StringSet.add y x) StringSet.empty lst

let next_var : unit -> string =
	let curr = ref 0 in
	fun () ->
		let res = "var" ^ (string_of_int (!curr)) in
		curr := !curr + 1;
		res

let rec eval (e : expr) (m : value VarMap.t) : value =
	let get_object_and_label (e1 : expr) (e2 : expr) : (value VarMap.t * string) =
		match eval e1 m, eval e2 m with
		| VObject o, VConstant(CString s) -> (o, s)
		| VObject o, _ -> raise(RuntimeError "Field label must be a string")
		| _, _ -> raise(RuntimeError "Field access must be to an object") in
	match e with
	| Var x -> (try VarMap.find x m with Not_found -> VConstant CUndefined)
	| Value v -> v
	| Let(x, e1, e2) ->
		let v1 = eval e1 m in
		eval e2 (VarMap.add x v1 m)
	| Call(e, args) ->
		(match eval e m with
		| VFunc(params, body) ->
			let foldf m' param arg =
				let v = eval arg m in
				VarMap.add param v m' in
			let m' = List.fold_left2 foldf m params args in
			eval body m'
		| _ -> raise(RuntimeError "This expression is not a function; it cannot be called"))
	| Access(e1, e2) ->
		let o, s = get_object_and_label e1 e2 in
		(try VarMap.find s o with Not_found ->
			(try
				let proto = VarMap.find "__proto__" o in
				eval (Access(Deref (Value proto), Value (VConstant (CString s)))) m
			with Not_found -> VConstant CUndefined))
	| Assignment(e1, e2, e3) ->
		let o, s = get_object_and_label e1 e2 in
		let v3 = eval e3 m in
		VObject (VarMap.add s v3 o)
	| Delete(e1, e2) ->
		let o, s = get_object_and_label e1 e2 in
		VObject(try VarMap.remove s o with Not_found -> o)
	| Ref e -> VRef(ref (eval e m))
	| Deref e -> (match eval e m with
		| VRef v -> !v
		| _ -> raise(RuntimeError "This expression is not a reference; it cannot be dereferenced"))
	| SetRef(e1, e2) -> (match eval e1 m with
		| VRef v -> v := (eval e2 m); VRef v
		| _ -> raise(RuntimeError "This expression is not a reference; it cannot be assigned to"))
	| If(e1, e2, e3) -> (match eval e1 m with
		| VConstant(CBool true) -> eval e2 m
		| VConstant(CBool false) -> eval e3 m
		| _ -> raise(RuntimeError "If condition must be a bool"))
	| Sequence(e1, e2) ->
		let _ = eval e1 m in
		eval e2 m
	| While(e1, e2) as w ->
		let e' = If(e1, Sequence(e2, w), Value (VConstant CUndefined)) in
		eval e' m
	| Err v -> raise(UserException v)
	| Break(l, e) ->
		let v = eval e m in
		raise(BreakException(l, v))
	| TryCatch(e1, x, e2) -> (try eval e1 m
		with UserException e -> eval e2 (VarMap.add x e m))
	| TryFinally(e1, e2) ->
		let v = try eval e1 m
			with (UserException _ | BreakException(_, _)) as e ->
				let _ = eval e2 m in
				raise e
		in
		let _ = eval e2 m in v
	| LabeledBlock(l, e) ->
		try eval e m
		with BreakException(l', v) when l = l' -> v

let eval e = eval e VarMap.empty