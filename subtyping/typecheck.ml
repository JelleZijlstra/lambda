open Ast

type lconstraint = Equals of ltype * ltype
	| IsSubType of ltype * ltype
	| IsLUB of ltype * ltype * ltype
	| HasLabel of ltype * string * ltype

type errmsg = string

module TypingContext = Map.Make(struct
	type t = string
	let compare = compare
end)

module ConstraintSet = Set.Make(struct
	type t = lconstraint
	let compare = compare
end)

module VariableSet = Set.Make(struct
	type t = string
	let compare = compare
end)

let verbose = true

let em = ConstraintSet.empty

type context = ltype TypingContext.t

exception TypeError of string

type type_cs = Type of ltype * ConstraintSet.t

let rec is_subtype (t1 : ltype) (t2 : ltype) : bool =
	match t1, t2 with
	| TInt, TInt | TBool, TBool | TUnit, TUnit -> true
	| TFunction(t1', t1''), TFunction(t2', t2'') ->
		is_subtype t2' t1' && is_subtype t1'' t2''
	| TSum(t1', t1''), TSum(t2', t2'')
	| TProduct(t1', t1''), TProduct(t2', t2'') ->
		is_subtype t1' t2' && is_subtype t1'' t2''
	| TRef t1', TRef t2' -> t1' = t2'
	| TRecord _, TRecord [] -> true
	| TRecord((l1, hd1)::tl1), TRecord((l2, hd2)::tl2) ->
		l1 = l2 && is_subtype hd1 hd2 && is_subtype (TRecord tl1) (TRecord tl2)
	| _, _ -> false

let rec lub (t1 : ltype) (t2 : ltype) : ltype =
	match t1, t2 with
	| TFunction(t1', t1''), TFunction(t2', t2'') ->
		TFunction(glb t1' t2', lub t1'' t2'')
	| TSum(t1', t1''), TSum(t2', t2'') ->
		TSum(lub t1' t2', lub t1'' t2'')
	| TProduct(t1', t1''), TProduct(t2', t2'') ->
		TProduct(lub t1' t2', lub t1'' t2'')
	| TRecord [], TRecord _ | TRecord _, TRecord [] -> TRecord []
	| TRecord((l1, hd1)::tl1), TRecord((l2, hd2)::tl2) when l1 = l2 ->
		(match lub (TRecord tl1) (TRecord tl2) with
		| TRecord lst -> TRecord((l1, lub hd1 hd2)::lst)
		| _ -> raise (TypeError "Must be a record"))
	| _, _ when t1 = t2 -> t1 (* covers TInt, TBool, TUnit, TRef *)
	| _, _ -> raise (TypeError("Cannot find lub of " ^ string_of_type t1 ^ " and " ^ string_of_type t2)) (* including TForAll *)
and glb (t1 : ltype) (t2 : ltype) : ltype =
	match t1, t2 with
	| TFunction(t1', t1''), TFunction(t2', t2'') ->
		TFunction(lub t1' t2', glb t1'' t2'')
	| TSum(t1', t1''), TSum(t2', t2'') ->
		TSum(glb t1' t2', glb t1'' t2'')
	| TProduct(t1', t1''), TProduct(t2', t2'') ->
		TProduct(glb t1' t2', glb t1'' t2'')
	| TRecord [], TRecord lst | TRecord lst, TRecord [] -> TRecord lst
	| TRecord((l1, hd1)::tl1), TRecord((l2, hd2)::tl2) when l1 = l2 ->
		(match glb (TRecord tl1) (TRecord tl2) with
		| TRecord lst -> TRecord((l1, glb hd1 hd2)::lst)
		| _ -> raise (TypeError "Must be a record"))
	| _, _ when t1 = t2 -> t1 (* covers TInt, TBool, TUnit, TRef *)
	| _, _ -> raise (TypeError("Cannot find glb of " ^ string_of_type t1 ^ " and " ^ string_of_type t2)) (* including TForAll *)

let rec free_variables (ty : ltype) : VariableSet.t = match ty with
	| Typevar t' -> VariableSet.singleton t'
	| TInt | TBool | TUnit -> VariableSet.empty
	| TProduct(t1, t2)
	| TSum(t1, t2)
	| TFunction(t1, t2) -> VariableSet.union (free_variables t1) (free_variables t2)
	| TRef t' -> free_variables t'
	| TRecord lst -> List.fold_left VariableSet.union VariableSet.empty (List.map (fun (_, t) -> free_variables t) lst)

let free_variables_context (c : context) =
	TypingContext.fold (fun _ t a -> VariableSet.union (free_variables t) a) c VariableSet.empty

let rec replace_in_type typevar new_type t =
	match t with
	| Typevar t' when t' = typevar -> new_type
	| Typevar _ | TInt | TBool | TUnit -> t
	| TFunction(t1, t2) -> TFunction(replace_in_type typevar new_type t1, replace_in_type typevar new_type t2)
	| TProduct(t1, t2) -> TProduct(replace_in_type typevar new_type t1, replace_in_type typevar new_type t2)
	| TSum(t1, t2) -> TSum(replace_in_type typevar new_type t1, replace_in_type typevar new_type t2)
	| TRef t -> TRef(replace_in_type typevar new_type t)
	| TRecord lst -> TRecord(List.map (fun (l, t) -> l, replace_in_type typevar new_type t) lst)

exception ImpossibleConstraint of string

type substitution = ltype TypingContext.t

let set_map f s = ConstraintSet.fold (fun elt s -> ConstraintSet.add (f elt) s) s ConstraintSet.empty

let replace_type typevar new_type =
	let mapf t = match t with
		| Equals(t1, t2) ->
			Equals(replace_in_type typevar new_type t1, replace_in_type typevar new_type t2)
		| IsSubType(t1, t2) ->
			IsSubType(replace_in_type typevar new_type t1, replace_in_type typevar new_type t2)
		| IsLUB(t1, t2, t3) ->
			IsLUB(replace_in_type typevar new_type t1, replace_in_type typevar new_type t2, replace_in_type typevar new_type t3)
		| HasLabel(t1, l, t2) ->
			HasLabel(replace_in_type typevar new_type t1, l, replace_in_type typevar new_type t2) in
	set_map mapf

let rec is_free_variable (t : string) (ty : ltype) = match ty with
	| Typevar t' -> t = t'
	| TInt | TBool | TUnit -> false
	| TProduct(t1, t2)
	| TSum(t1, t2)
	| TFunction(t1, t2) -> is_free_variable t t1 || is_free_variable t t2
	| TRef t' -> is_free_variable t t'
	| TRecord lst -> List.exists (fun (l, t') -> is_free_variable t t') lst

let rec unify (cs : ConstraintSet.t) : substitution =
	try (let chosen = ConstraintSet.choose cs in
		let new_set = ConstraintSet.remove chosen cs in
		match chosen with
		| Equals(t1, t2) when t1 = t2 -> unify new_set
		| Equals(t', Typevar t)
		| Equals(Typevar t, t') when (not(is_free_variable t t')) ->
			let new_cs = replace_type t t' new_set in
			let rest = unify new_cs in
			TypingContext.add t t' rest
		| Equals(TProduct(t0, t1), TProduct(t0', t1'))
		| Equals(TSum(t0, t1), TSum(t0', t1'))
		| Equals(TFunction(t0, t1), TFunction(t0', t1')) ->
			let new_cs = ConstraintSet.add (Equals(t0, t0')) new_set in
			let new_cs = ConstraintSet.add (Equals(t1, t1')) new_cs in
			unify new_cs
		| Equals(TRef t0, TRef t0') ->
			let new_cs = ConstraintSet.add (Equals(t0, t0')) new_set in
			unify new_cs
		| HasLabel(TRecord lst, l, t) ->
			let t' = try List.assoc l lst with Not_found ->
				let msg = "Label " ^ l ^ " does not exist in type: " ^ string_of_type (TRecord lst) in
				raise(ImpossibleConstraint msg) in
			let new_cs = ConstraintSet.add (IsSubType(t, t')) new_set in
			unify new_cs
		| HasLabel(Typevar _ as t, l, tl) ->
			let new_cs = ConstraintSet.add (IsSubType(t, TRecord([(l, tl)]))) new_set in
			unify new_cs
		| HasLabel(t, l, t') -> raise(ImpossibleConstraint("Type is not a record type: " ^ string_of_type t))
		| IsSubType(t1, t2) when t1 = t2 -> unify new_set
		| IsSubType(TSum(t0, t1), TSum(t0', t1'))
		| IsSubType(TProduct(t0, t1), TProduct(t0', t1')) ->
			let new_cs = ConstraintSet.add (IsSubType(t0, t0')) new_set in
			let new_cs = ConstraintSet.add (IsSubType(t1, t1')) new_cs in
			unify new_cs
		| IsSubType(TFunction(t0, t1), TFunction(t0', t1')) ->
			let new_cs = ConstraintSet.add (IsSubType(t0', t0)) new_set in
			let new_cs = ConstraintSet.add (IsSubType(t1, t1')) new_cs in
			unify new_cs
		| IsSubType(TRecord _, TRecord []) -> unify new_set
		| IsSubType(TRecord((l1, hd1)::tl1), TRecord((l2, hd2)::tl2)) when l1 = l2 ->
			let new_cs = ConstraintSet.add (IsSubType(hd1, hd2)) new_set in
			let new_cs = ConstraintSet.add (IsSubType(TRecord tl1, TRecord tl2)) new_cs in
			unify new_cs
		| IsSubType((Typevar tv) as t2, ((TInt | TBool | TUnit | TRef _) as t1))
		| IsSubType((TInt | TBool | TUnit | TRef _) as t2, ((Typevar tv) as t1)) ->
			let new_cs = ConstraintSet.add (Equals(t1, t2)) new_set in
			unify new_cs
		| IsSubType((Typevar tv) as t1, (TProduct(_, _) as t2)) ->
			let t' = TProduct(Ast.new_typevar(), Ast.new_typevar()) in
			let new_cs = ConstraintSet.add (Equals(t1, t')) new_set in
			let new_cs = ConstraintSet.add (IsSubType(t', t2)) new_cs in
			unify new_cs
		| IsSubType((Typevar tv) as t1, (TSum(_, _) as t2)) ->
			let t' = TSum(Ast.new_typevar(), Ast.new_typevar()) in
			let new_cs = ConstraintSet.add (Equals(t1, t')) new_set in
			let new_cs = ConstraintSet.add (IsSubType(t', t2)) new_cs in
			unify new_cs
		| IsSubType((Typevar tv) as t1, (TFunction(_, _) as t2)) ->
			let t' = TFunction(Ast.new_typevar(), Ast.new_typevar()) in
			let new_cs = ConstraintSet.add (Equals(t1, t')) new_set in
			let new_cs = ConstraintSet.add (IsSubType(t', t2)) new_cs in
			unify new_cs
		| IsSubType(TProduct(_, _) as t2, ((Typevar tv) as t1)) ->
			let t' = TProduct(Ast.new_typevar(), Ast.new_typevar()) in
			let new_cs = ConstraintSet.add (Equals(t1, t')) new_set in
			let new_cs = ConstraintSet.add (IsSubType(t2, t')) new_cs in
			unify new_cs
		| IsSubType(TSum(_, _) as t2, ((Typevar tv) as t1)) ->
			let t' = TSum(Ast.new_typevar(), Ast.new_typevar()) in
			let new_cs = ConstraintSet.add (Equals(t1, t')) new_set in
			let new_cs = ConstraintSet.add (IsSubType(t2, t')) new_cs in
			unify new_cs
		| IsSubType(TFunction(_, _) as t2, ((Typevar tv) as t1)) ->
			let t' = TFunction(Ast.new_typevar(), Ast.new_typevar()) in
			let new_cs = ConstraintSet.add (Equals(t1, t')) new_set in
			let new_cs = ConstraintSet.add (IsSubType(t2, t')) new_cs in
			unify new_cs
		| IsSubType(Typevar _ as t1, (TRecord(_) as t2))
		| IsSubType(TRecord _ as t1, (Typevar(_) as t2))
		| IsSubType(Typevar _ as t1, (Typevar _ as t2)) ->
			(* Yuck *)
			let new_cs = ConstraintSet.add (Equals(t1, t2)) new_set in
			unify new_cs
		| IsSubType(t1, t2) ->
			let types = string_of_type t1 ^ " and " ^ string_of_type t2 in
			raise(ImpossibleConstraint("Cannot satisfy subtyping relationship: " ^ types))
		| IsLUB(t1, t2, t3) ->
			let lub = lub t2 t3 in
			let new_cs = ConstraintSet.add (IsSubType(t1, lub)) new_set in
			unify new_cs
		| Equals(t1, t2) ->
			let types = string_of_type t1 ^ " and " ^ string_of_type t2 in
			raise(ImpossibleConstraint("Cannot unify types: " ^ types)))
	with Not_found -> TypingContext.empty

let rec apply_substitution (s : substitution) (t : ltype) (b : VariableSet.t) : ltype =
	match t with
	| TInt | TBool | TUnit -> t
	| TRef t' -> TRef(apply_substitution s t' b)
	| TFunction(t1, t2) -> TFunction(apply_substitution s t1 b, apply_substitution s t2 b)
	| TSum(t1, t2) -> TSum(apply_substitution s t1 b, apply_substitution s t2 b)
	| TProduct(t1, t2) -> TProduct(apply_substitution s t1 b, apply_substitution s t2 b)
	| Typevar tv when not (VariableSet.mem tv b) ->
		(try apply_substitution s (TypingContext.find tv s) b
		with Not_found -> t)
	| Typevar tv -> t
	| TRecord(lst) -> TRecord(List.map (fun (l, t) -> (l, apply_substitution s t b)) lst)

let rec get_type (e : expr) (c : context) : type_cs =
	match e with
	| Int _ -> Type(TInt, em)
	| Bool _ -> Type(TBool, em)
	| Unit -> Type(TUnit, em)
	| Var x ->
		(try Type(TypingContext.find x c, em)
			with Not_found -> raise (TypeError("Unbound variable: " ^ x)))
	| Let(x, t, e1, e2) ->
		let Type(t1, cs1) = get_type e1 c in
		let subst = unify cs1 in
		let t1' = apply_substitution subst t1 VariableSet.empty in
		let new_tc = TypingContext.add x t1' c in
		let Type(t2, cs2) = get_type e2 new_tc in
		Type(t2, ConstraintSet.add (Equals(t, t1')) (ConstraintSet.union cs1 cs2))
	| LetRec(x, t, e1, e2) ->
		let e1_c = TypingContext.add x t c in
		let Type(t1, cs1) = get_type e1 e1_c in
		let subst = unify cs1 in
		let t1' = apply_substitution subst t1 VariableSet.empty in
		let new_tc = TypingContext.add x t1' c in
		let Type(t2, cs2) = get_type e2 new_tc in
		Type(t2, ConstraintSet.add (Equals(t, t1')) (ConstraintSet.union cs1 cs2))
	| Binop(_, e1, e2) ->
		let Type(t1, cs1) = get_type e1 c in
		let Type(t2, cs2) = get_type e2 c in
		Type(TInt, ConstraintSet.add (IsSubType(t1, TInt))
			(ConstraintSet.add (IsSubType(t2, TInt))
				(ConstraintSet.union cs1 cs2)))
	| Boolbinop(_, e1, e2) ->
		let Type(t1, cs1) = get_type e1 c in
		let Type(t2, cs2) = get_type e2 c in
		Type(TBool, ConstraintSet.add (IsSubType(t1, TInt))
			(ConstraintSet.add (IsSubType(t2, TInt))
				(ConstraintSet.union cs1 cs2)))
	| Unop(_, e) ->
		let Type(t, cs) = get_type e c in
		Type(TInt, ConstraintSet.add (IsSubType(t, TInt)) cs)
	| Application(e1, e2) ->
		let Type(t1, cs1) = get_type e1 c in
		let Type(t2, cs2) = get_type e2 c in
		let new_type = Ast.new_typevar() in
		let new_cs = ConstraintSet.add (IsSubType(t1, TFunction(t2, new_type))) (ConstraintSet.union cs1 cs2) in
		Type(new_type, new_cs)
	| Abstraction(arg, t, body) ->
		let Type(t', cs) = get_type body (TypingContext.add arg t c) in
		Type(TFunction(t, t'), cs)
	| Fix e ->
		let Type(t, cs) = get_type e c in
		let new_type = Ast.new_typevar() in
		let new_cs = ConstraintSet.add (Equals(t, TFunction(new_type, new_type))) cs in
		Type(new_type, new_cs)
	| If(e1, e2, e3) ->
		let Type(t1, cs1) = get_type e1 c in
		let Type(t2, cs2) = get_type e2 c in
		let Type(t3, cs3) = get_type e3 c in
		let tv = Ast.new_typevar() in
		let new_cs = ConstraintSet.union cs1 (ConstraintSet.union cs2 cs3) in
		let new_cs = ConstraintSet.add (IsSubType(t1, TBool)) (ConstraintSet.add (IsLUB(tv, t2, t3)) new_cs) in
		Type(tv, new_cs)
	| Pair(e1, e2) ->
		let Type(t1, cs1) = get_type e1 c in
		let Type(t2, cs2) = get_type e2 c in
		Type(TProduct(t1, t2), ConstraintSet.union cs1 cs2)
	| Projection(b, e) ->
		let Type(t, cs) = get_type e c in
		let left_typevar = Ast.new_typevar() in
		let right_typevar = Ast.new_typevar() in
		let new_constraint = IsSubType(t, TProduct(left_typevar, right_typevar)) in
		let my_type = if b then right_typevar else left_typevar in
		Type(my_type, ConstraintSet.add new_constraint cs)
	| Injection(b, e) ->
		let Type(t, cs) = get_type e c in
		let other_typevar = Ast.new_typevar() in
		let my_type = if b then TSum(other_typevar, t) else TSum(t, other_typevar) in
		Type(my_type, cs)
	| Case(e1, e2, e3) ->
		let Type(t1, cs1) = get_type e1 c in
		let Type(t2, cs2) = get_type e2 c in
		let Type(t3, cs3) = get_type e3 c in
		let tv2a = Ast.new_typevar() in
		let tv3a = Ast.new_typevar() in
		let tv_res = Ast.new_typevar() in
		let new_cs = ConstraintSet.union cs1 (ConstraintSet.union cs2 cs3) in
		let with_sum = ConstraintSet.add (IsSubType(t1, TSum(tv2a, tv3a))) new_cs in
		let with_left = ConstraintSet.add (IsSubType(t2, TFunction(tv2a, tv_res))) with_sum in
		let with_right = ConstraintSet.add (IsSubType(t3, TFunction(tv3a, tv_res))) with_left in
		Type(tv_res, with_right)
	| Allocation(e) ->
		let Type(t, cs) = get_type e c in Type(TRef t, cs)
	| Dereference(e) ->
		let Type(t, cs) = get_type e c in
		let tv = Ast.new_typevar() in
		let new_cs = ConstraintSet.add (IsSubType(t, TRef tv)) cs in
		Type(tv, new_cs)
	| Assignment(e1, e2) ->
		let Type(t1, cs1) = get_type e1 c in
		let Type(t2, cs2) = get_type e2 c in
		let new_cs = ConstraintSet.add (IsSubType(t1, TRef t2)) (ConstraintSet.union cs1 cs2) in
		Type(TUnit, new_cs)
	| Sequence(e1, e2) ->
		let Type(t1, cs1) = get_type e1 c in
		let Type(t2, cs2) = get_type e2 c in
		Type(t2, ConstraintSet.union cs1 cs2)
	| Reference e ->
		let Type(t, cs) = get_type (!e) c in Type(TRef t, cs)
 	| Record lst ->
		let foldf (rest, cs) (l, e) =
			let Type(t, cs') = get_type e c in (l, t)::rest, ConstraintSet.union cs cs' in
		let t, cs = List.fold_left foldf ([], ConstraintSet.empty) lst in
		Type(TRecord t, cs)
	| Member(e, l) ->
		let Type(t, cs) = get_type e c in
		let tv = Ast.new_typevar() in
		let new_cs = ConstraintSet.add (HasLabel(t, l, tv)) cs in
		Type(tv, new_cs)

let print_cs cs = ConstraintSet.fold (fun e a -> (match e with
	| Equals(t1, t2) -> "\t" ^ string_of_type t1 ^ " = " ^ string_of_type t2
	| IsSubType(t1, t2) -> "\t" ^ string_of_type t1 ^ " is a subtype of " ^ string_of_type t2
	| IsLUB(t1, t2, t3) -> "\t" ^ string_of_type t1 ^ " is the LUB of " ^ string_of_type t2 ^ " and " ^ string_of_type t3
	| HasLabel(t1, l, t2) -> "\t" ^ string_of_type t1 ^ " has label " ^ l ^ " of type " ^ string_of_type t2) ^ "\n" ^ a) cs ""

let typecheck e =
	try let Type(t, cs) = get_type e TypingContext.empty in
		(try
			if verbose then (Printf.printf "%s\n" (print_cs cs);
				Printf.printf "%s\n" (string_of_type t);
				flush_all ());
			let subst = unify cs in
			if verbose then
				let res_t = apply_substitution subst t VariableSet.empty in
				Printf.printf "%s\n" (string_of_type res_t)
			else ignore subst;
			None
		with ImpossibleConstraint e -> Some e)
	with TypeError e -> Some e
