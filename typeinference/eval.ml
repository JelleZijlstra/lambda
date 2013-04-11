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
	| PVariable _ | PConstructor _ | PAnything | PBool _ | PInt _ -> false

let rec is_free_variable (var : string) (code : expr) : bool = match code with
	| Var x | Constructor x -> var = x
	| Application(e1, e2)
	| Pair(e1, e2) -> is_free_variable var e1 || is_free_variable var e2
	| Abstraction(arg, _, body) -> (arg <> var) && (is_free_variable var body)
	| Int _ | Bool _ | Reference _ | Unit | ADTInstance(_, _) -> false
	| Assignment(e1, e2)
	| Sequence(e1, e2)
	| Binop(_, e1, e2)
	| Boolbinop(_, e1, e2) -> is_free_variable var e1 || is_free_variable var e2
	| Case(e1, e2, e3)
	| If(e1, e2, e3) -> is_free_variable var e1 || is_free_variable var e2 || is_free_variable var e3
	| Allocation e
	| Dereference e
	| Unop(_, e)
	| Member(e, _)
	| Fix e -> is_free_variable var e
	| Projection(_, e)
	| LetADT(_, _, _, e)
	| TypeSynonym(_, _, e)
	| Injection(_, e) -> is_free_variable var e
	| Record lst -> List.exists (fun (l, e) -> is_free_variable var e) lst
	| Let(x, t, e1, e2) -> is_free_variable var e1 || (x <> var && is_free_variable var e2)
	| LetRec(x, t, e1, e2) -> (x <> var) && (is_free_variable var e1 || is_free_variable var e2)
	| Match(e, lst) ->
		let existsf (p, e) = not (exists_in_pattern var p) && is_free_variable var e in
		is_free_variable var e || List.exists existsf lst

let rec substitute (code : expr) (var : string) (replacement : expr) : expr = match code with
	| Var(x) -> if x = var then replacement else Var x
	| Constructor n -> if n = var then replacement else Constructor n
	| Application(e1, e2) -> Application(substitute e1 var replacement, substitute e2 var replacement)
	| Int _ | Bool _ | Reference _ | Unit -> code
	| Binop(op, e1, e2) -> Binop(op, substitute e1 var replacement, substitute e2 var replacement)
	| Boolbinop(op, e1, e2) -> Boolbinop(op, substitute e1 var replacement, substitute e2 var replacement)
	| If(e1, e2, e3) -> If(substitute e1 var replacement, substitute e2 var replacement, substitute e3 var replacement)
	| Unop(op, e) -> Unop(op, substitute e var replacement)
	| Fix e -> Fix(substitute e var replacement)
	| Allocation e -> Allocation(substitute e var replacement)
	| Assignment(e1, e2) -> Assignment(substitute e1 var replacement, substitute e2 var replacement)
	| Dereference e -> Dereference(substitute e var replacement)
	| Abstraction(arg, t, body) ->
		if arg = var then Abstraction(arg, t, body)
		else if not (is_free_variable arg replacement)
			then Abstraction(arg, t, substitute body var replacement)
			else let var_name = get_unique_var_name () in
				Abstraction(var_name, t, substitute (substitute body arg (Var var_name)) var replacement)
	| Pair(e1, e2) -> Pair(substitute e1 var replacement, substitute e2 var replacement)
	| Projection(b, e) -> Projection(b, substitute e var replacement)
	| Injection(b, e) -> Injection(b, substitute e var replacement)
	| Case(e1, e2, e3) -> Case(substitute e1 var replacement, substitute e2 var replacement, substitute e3 var replacement)
	| Sequence(e1, e2) -> Sequence(substitute e1 var replacement, substitute e2 var replacement)
	| Record lst -> Record(List.map (fun (l, e) -> l, substitute e var replacement) lst)
	| Member(e, l) -> Member(substitute e var replacement, l)
	| Let(x, t, e1, e2) when x = var -> Let(x, t, substitute e1 var replacement, e2)
	| Let(x, t, e1, e2) -> Let(x, t, substitute e1 var replacement, substitute e2 var replacement)
	| LetRec(x, t, e1, e2) when x = var -> code
	| LetRec(x, t, e1, e2) -> LetRec(x, t, substitute e1 var replacement, substitute e2 var replacement)
	| TypeSynonym(_, _, e) (* Safe to just erase these things from the expression *)
	| LetADT(_, _, _, e) -> substitute e var replacement
	| ADTInstance(v1, v2) -> ADTInstance(v1, substitute v2 var replacement)
	| Match(e, lst) ->
		let mapf (p, e) = if exists_in_pattern var p then (p, e) else (p, substitute e var replacement) in
		Match(substitute e var replacement, List.map mapf lst)

let rec eval (e : expr) (s : semantics) : expr = match e with
	| Var x -> failwith ("Unbound variable: " ^ x)
	| Abstraction(_, _, _) | Reference _ -> e
	| Int n -> Int n
	| Bool b -> Bool b
	| Unit -> Unit
	| Binop(op, e1, e2) ->
		let e1' = eval e1 s in
		let e2' = eval e2 s in
		(match e1', e2' with
		| Int n1, Int n2 -> Int(f_of_binop op n1 n2)
		| Int _, _ -> failwith("Invalid operand to binary expression: " ^ string_of_expr e2')
		| _, _ -> failwith("Invalid operand to binary expression: " ^ string_of_expr e1'))
	| Boolbinop(op, e1, e2) ->
		let e1' = eval e1 s in
		let e2' = eval e2 s in
		(match e1', e2' with
		| Int n1, Int n2 -> Bool(f_of_bool_binop op n1 n2)
		| Int _, _ -> failwith("Invalid operand to binary expression: " ^ string_of_expr e2')
		| _, _ -> failwith("Invalid operand to binary expression: " ^ string_of_expr e1'))
	| Unop(op, e) ->
		let e' = eval e s in
		(match e' with
		| Int n -> Int(f_of_unop op n)
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
		| Constructor _ | ADTInstance(_, _) -> ADTInstance(e1', e2')
		| _ -> failwith "This expression is not a function; it cannot be applied")
	| Fix(Abstraction(arg, _, body)) ->
		eval (substitute body arg e) s
	| Fix _ -> failwith "Invalid use of fix"
	| If(e1, e2, e3) -> (match eval e1 s with
		| Bool true -> eval e2 s
		| Bool false -> eval e3 s
		| _ -> failwith "If block must contain a bool")
	| Pair(e1, e2) ->
		let e1' = eval e1 s in
		let e2' = eval e2 s in
		Pair(e1', e2')
	| Projection(b, e) -> (match eval e s with
		| Pair(e1, e2) -> if b then e2 else e1
		| _ -> failwith "This expression is not a product; it cannot be projected")
	| Injection(b, e) -> Injection(b, eval e s)
	| Case(e1, e2, e3) -> (match eval e1 s with
		| Injection(false, e') -> eval (Application(e2, e')) s
		| Injection(true, e') -> eval (Application(e3, e')) s
		| _ -> failwith "This expression is not a sum; it cannot be matched on")
	| Assignment(e1, e2) -> (match eval e1 s with
		| Reference r -> r := (eval e2 s)
		| _ -> failwith "This expression is not a reference; it cannot be assigned to"); Unit
	| Allocation e -> Reference(ref (eval e s))
	| Dereference e -> (match eval e s with
		| Reference r -> !r
		| _ -> failwith "This expression is not a reference; it cannot be dereferenced")
	| Sequence(e1, e2) -> let _ = eval e1 s in eval e2 s
	| Record lst -> Record(List.map (fun (l, e) -> l, eval e s) lst)
	| Member(e, l) -> (match eval e s with
		| Record lst -> (try List.assoc l lst
			with Not_found -> failwith("Unknown label: " ^ l))
		| _ -> failwith "This expression is not a record; member access is not possible")
	| Let(x, t, e1, e2) ->
		let e1' = eval e1 s in
		let substituted = substitute e2 x e1' in
		eval substituted s
	| LetRec(x, t, e1, e2) ->
		let e1' = eval (Fix(Abstraction(x, t, e1))) s in
		let substituted = substitute e2 x e1' in
		eval substituted s
	| ADTInstance(n, lst) -> ADTInstance(n, lst)
	| TypeSynonym(_, _, e) (* Type declarations are ignored at runtime *)
	| LetADT(_, _, _, e) -> eval e s
	| Constructor n -> Constructor n
	| Match(e, lst) ->
		let match_expr = eval e s in
		let rec eval_pattern p e = match p with
			| PAnything -> Some []
			| PVariable v -> Some[(v, e)]
			| PInt n -> (match e with
				| Int n' when n = n' -> Some []
				| _ -> None)
			| PBool b -> (match e with
				| Bool b' when b = b' -> Some []
				| _ -> None)
			| PPair(p1, p2) -> (match e with
				| Pair(e1, e2) -> (match eval_pattern p1 e1, eval_pattern p2 e2 with
					| Some lst1, Some lst2 -> Some(lst1 @ lst2)
					| _, _ -> None)
				| _ -> failwith "Pair must be matched with pair")
			| PConstructor v -> (match e with
				| Constructor v' when v = v' -> Some []
				| _ -> None)
			| PApplication(p1, p2) -> (match e with
				| ADTInstance(v1, v2) -> (match eval_pattern p1 v1, eval_pattern p2 v2 with
					| Some lst1, Some lst2 -> Some(lst1 @ lst2)
					| _, _ -> None)
				| _ -> None) in
		let rec eval_match lst = match lst with
			| [] -> failwith "Inexhaustive pattern matching"
			| (p, case_body)::tl -> (match eval_pattern p match_expr with
				| None -> eval_match tl
				| Some lst ->
					let foldf e (x, v) = substitute e x v in
					let e' = List.fold_left foldf case_body lst in
					eval e' s) in
		eval_match lst

let eval_cbv e = eval e CBV
let eval_cbn e = eval e CBN
