open Ast

exception RuntimeError of string
exception UserException of value
exception BreakException of string * value

let rec eval (e : expr) (m : value VarMap.t) : value =
	let get_object_and_label (e1 : expr) (e2 : expr) : (value VarMap.t * string) =
		match eval e1 m, eval e2 m with
		| VObject o, VConstant(CString s) -> (o, s)
		| VObject o, _ -> raise(RuntimeError "Field label must be a string")
		| _, _ -> raise(RuntimeError "Field access must be to an object") in
	match e with
	| Var x -> (try VarMap.find x m with Not_found -> VConstant CUndefined)
	| Func(args, body) -> VClosure(args, m, body)
	| Value v -> v
	| Let(x, e1, e2) ->
		let v1 = eval e1 m in
		eval e2 (VarMap.add x v1 m)
	| Call(e, args) ->
		(match eval e m with
		| VClosure(params, new_m, body) ->
			let foldf m' param arg =
				let v = eval arg m in
				VarMap.add param v m' in
			let m' = List.fold_left2 foldf new_m params args in
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
		(try eval e m
		with BreakException(l', v) when l = l' -> v)
	| Log e ->
		let v = eval e m in
		Printf.printf "%s\n" (string_of_value v);
		VConstant CUndefined
	| Binop(bo, e1, e2) ->
		(* Ensure order of execution *)
		let v1 = eval e1 m in
		let v2 = eval e2 m in
		eval_binop bo v1 v2
and eval_binop (bo : binop) (v1 : value) (v2 : value) : value =
	let f_of_int_binop (bo : binop) : int -> int -> int =
		match bo with
		| Add -> (+)
		| Subtract -> (-)
		| Multiply -> ( * )
		| Divide -> (/)
		| _ -> failwith "Impossible" in
	let f_of_int_bool_binop (bo : binop) : int -> int -> bool =
		match bo with
		| Less -> (<)
		| LE -> (<=)
		| Greater -> (>)
		| GE -> (>=)
		| _ -> failwith "Impossible" in
	let eval_int_binop (bo : binop) (v1 : value) (v2 : value) : value =
		match v1, v2 with
		| VConstant(CInt n1), VConstant(CInt n2) -> VConstant(CInt(f_of_int_binop bo n1 n2))
		| _, _ -> raise(RuntimeError(string_of_binop bo ^ " is only defined on ints")) in
	let eval_int_bool_binop (bo : binop) (v1 : value) (v2 : value) : value =
		match v1, v2 with
		| VConstant(CInt n1), VConstant(CInt n2) -> VConstant(CBool(f_of_int_bool_binop bo n1 n2))
		| _, _ -> raise(RuntimeError(string_of_binop bo ^ " is only defined on ints")) in
	match bo, v1, v2 with
	| Add, _, _ | Subtract, _, _ | Multiply, _, _ | Divide, _, _ ->
		eval_int_binop bo v1 v2
	| Less, _, _ | Greater, _, _ | LE, _, _ | GE, _, _ ->
		eval_int_bool_binop bo v1 v2
	| Concat, VConstant(CString s1), VConstant(CString s2) -> VConstant(CString(s1 ^ s2))
	| Concat, _, _ -> raise(RuntimeError("++ is only defined on strings"))
	| Equals, _, _ -> VConstant(CBool(v1 = v2))
	| NEquals, _, _ -> VConstant(CBool(v1 <> v2))

let eval e = eval e VarMap.empty