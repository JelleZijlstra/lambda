open Ast

type semantics = CBV | CBN

(* TODO: runtime context (e.g., variable mapping)
module Context : sig
	type t
end = struct
	type t =
end
 *)

let get_unique_var_name : unit -> string =
	let current = ref 0 in
	fun () ->
		let index = !current in
		current := index + 1;
		"'" ^ (string_of_int index)

let rec exists_in_pattern var p = match p with
	| PVariable x | PConstructor x when x = var -> true
	| PApplication(p1, p2)
	| PPair(p1, p2) -> exists_in_pattern var p1 || exists_in_pattern var p2
	| PGuarded(p, e) -> exists_in_pattern var p
	| PAs(p, x) -> exists_in_pattern var p || x = var
	| PVariable _ | PConstructor _ | PAnything | PBool _ | PInt _ | PString _ -> false

type bound_vars = value VarMap.t

let rec eval' (e, t : typed_expr) (s : bound_vars) : value = match e with
	| Var x -> (match VarMap.find x s with
		| VDummy(e, s) -> eval' e s
		| v -> v)
	| Dummy v -> v
	| Wrapped e -> eval' e s
	| Abstraction(x, _, e) -> VAbstraction(x, s, e)
	| Error e -> VError e
	| Int n -> VInt n
	| Bool b -> VBool b
	| String s -> VString s
	| Unit -> VUnit
	| Binop(op, e1, e2) ->
		let e1' = eval' e1 s in
		let e2' = eval' e2 s in
		(match e1', e2' with
		| VInt n1, VInt n2 -> VInt(f_of_binop op n1 n2)
		| VInt _, _ -> failwith("Invalid operand to binary expression: " ^ string_of_value e2')
		| _, _ -> failwith("Invalid operand to binary expression: " ^ string_of_value e1'))
	| Boolbinop(op, e1, e2) ->
		let e1' = eval' e1 s in
		let e2' = eval' e2 s in
		(match e1', e2' with
		| VInt n1, VInt n2 -> VBool(f_of_bool_binop op n1 n2)
		| VInt _, _ -> failwith("Invalid operand to binary expression: " ^ string_of_value e2')
		| _, _ -> failwith("Invalid operand to binary expression: " ^ string_of_value e1'))
	| Application(e1, e2) ->
		let e1' = eval' e1 s in
		let e2' = eval' e2 s in
		(match e1' with
		| VAbstraction(arg, s, body) ->
			eval' body (VarMap.add arg e2' s)
		| VConstructor _ | VADTInstance(_, _) -> VADTInstance(e1', e2')
		| VBuiltin b -> b e2'
		| _ -> failwith "This expression is not a function; it cannot be applied")
	| Fix(Abstraction(arg, _, body), _) ->
		eval' body (VarMap.add arg (VDummy((e, t), s)) s)
	| Fix _ -> failwith "Invalid use of fix"
	| If(e1, e2, e3) -> (match eval' e1 s with
		| VBool true -> eval' e2 s
		| VBool false -> eval' e3 s
		| _ -> failwith "If block must contain a bool")
	| Pair(e1, e2) ->
		let e1' = eval' e1 s in
		let e2' = eval' e2 s in
		VPair(e1', e2')
	| Projection(b, e) -> (match eval' e s with
		| VPair(e1, e2) -> if b then e2 else e1
		| _ -> failwith "This expression is not a product; it cannot be projected")
	| Injection(b, e) -> VInjection(b, eval' e s)
	| Case(e1, e2, e3) -> (match eval' e1 s with
		| VInjection(false, e') -> eval' (Application(e2, (Dummy e', ref None)), ref None) s
		| VInjection(true, e') -> eval' (Application(e3, (Dummy e', ref None)), ref None) s
		| _ -> failwith "This expression is not a sum; it cannot be matched on")
	| Assignment(e1, e2) -> (match eval' e1 s with
		| VReference r -> r := (eval' e2 s)
		| _ -> failwith "This expression is not a reference; it cannot be assigned to"); VUnit
	| Allocation e -> VReference(ref (eval' e s))
	| Dereference e -> (match eval' e s with
		| VReference r -> !r
		| _ -> failwith "This expression is not a reference; it cannot be dereferenced")
	| In(SingleExpression e1, e2)
	| Sequence(e1, e2) -> let _ = eval' e1 s in eval' e2 s
	| Record lst -> VRecord(VarMap.map (fun e -> eval' e s) lst)
	| Member(e, l) -> (match eval' e s with
		| VRecord lst -> (try VarMap.find l lst
			with Not_found -> failwith("Unknown label: " ^ l))
		| VModule(_, lst) -> (try VarMap.find l lst
			with Not_found -> failwith("Unknown module member: " ^ l))
		| _ -> failwith "This expression is not a record; member access is not possible")
	| In(Let(x, t, e1), e2) ->
		let e1' = eval' e1 s in
		eval' e2 (VarMap.add x e1' s)
	| In(LetRec(x, t, e1), e2) ->
		let e1' = eval' (Fix(Abstraction(x, t, e1), t), t) s in
		eval' e2 (VarMap.add x e1' s)
	| In(TypeSynonym(_, _), e) (* Type declarations are ignored at runtime *)
	| In(LetADT(_, _, _), e) -> eval' e s
	| Constructor n -> VConstructor n
	| ConstructorMember(e, l) -> VConstructor l (* TODO: proper namespacing *)
	| Match(e, lst) ->
		let match_expr = eval' e s in
		let add_list = List.fold_left (fun e (x, v) -> VarMap.add x v e) in
		let rec eval_pattern p e = match p with
			| PAnything -> Some []
			| PVariable v -> Some[(v, e)]
			| PInt n -> (match e with
				| VInt n' when n = n' -> Some []
				| _ -> None)
			| PBool b -> (match e with
				| VBool b' when b = b' -> Some []
				| _ -> None)
			| PString s -> (match e with
				| VString s' when s = s' -> Some []
				| _ -> None)
			| PPair(p1, p2) -> (match e with
				| VPair(e1, e2) -> (match eval_pattern p1 e1, eval_pattern p2 e2 with
					| Some lst1, Some lst2 -> Some(lst1 @ lst2)
					| _, _ -> None)
				| _ -> failwith "Pair must be matched with pair")
			| PConstructor v -> (match e with
				| VConstructor v' when v = v' -> Some []
				| _ -> None)
			| PApplication(p1, p2) -> (match e with
				| VADTInstance(v1, v2) -> (match eval_pattern p1 v1, eval_pattern p2 v2 with
					| Some lst1, Some lst2 -> Some(lst1 @ lst2)
					| _, _ -> None)
				| _ -> None)
			| PGuarded(p, e') -> (match eval_pattern p e with
				| None -> None
				| Some lst ->
					let s = add_list s lst in
					match eval' e' s with
					| VBool true -> Some lst
					| VBool false -> None
					| _ -> failwith "Pattern guard must return a bool")
			| PAs(p, x) -> (match eval_pattern p e with
				| None -> None
				| Some lst -> Some((x, e)::lst))
		in
		let rec eval_match lst = match lst with
			| [] -> failwith "Inexhaustive pattern matching"
			| (p, case_body)::tl -> (match eval_pattern p match_expr with
				| None -> eval_match tl
				| Some lst ->
					let s = add_list s lst in
					eval' case_body s) in
		eval_match lst
	| In(Open m, e) -> eval' e (do_open m s)
	| Module(Some(TModule t), lst) ->
		let rec loop lst s = match lst with
			| [] -> VarMap.empty
			| Let(x, _, e)::tl ->
				let f = VarMap.add x (eval' e s) in
				f (loop tl (f s))
			| LetRec(x, t, e)::tl ->
				let v = eval' (Fix(Abstraction(x, t, e), t), t) s in
				let f = VarMap.add x v in
				f (loop tl (f s))
			| SingleExpression e::tl ->
				let f = VarMap.add "__result__" (eval' e s) in
				f (loop tl s)
			| Open m::tl ->
				let s' = do_open m s in
				loop tl s'
			| (LetADT(_, _, _) | TypeSynonym(_, _) | Import _)::tl -> loop tl s
		in
		VModule(t, loop lst s)
	| Module(_, _) | In(Import _, _) -> failwith "impossible"
and do_open m s = match VarMap.find m s with
	| VModule(t, lst) ->
		VarMap.fold VarMap.add lst s
	| _ -> failwith "Invalid open"

let eval e =
	eval' e (VarMap.map (fun b -> VBuiltin b) Builtin.builtin_values)
