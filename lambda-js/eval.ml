open Ast

exception RuntimeError of string

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
		(try VarMap.find s o with Not_found -> VConstant CUndefined)
	| Assignment(e1, e2, e3) ->
		let o, s = get_object_and_label e1 e2 in
		let v3 = eval e3 m in
		VObject (VarMap.add s v3 o)
	| Delete(e1, e2) ->
		let o, s = get_object_and_label e1 e2 in
		VObject(try VarMap.remove s o with Not_found -> o)

let eval e = eval e VarMap.empty