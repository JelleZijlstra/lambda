open Ast

type lconstraint = Equals of ltype * ltype | HasLabel of ltype * string * ltype

type kconstraint = KEquals of kind * kind

type errmsg = string

module VariableMap = Map.Make(struct
	type t = string
	let compare = compare
end)

module ConstraintSet = Set.Make(struct
	type t = lconstraint
	let compare = compare
end)

module KindConstraintSet = Set.Make(struct
	type t = kconstraint
	let compare = compare
end)

module VariableSet = Set.Make(struct
	type t = string
	let compare = compare
end)

module Context = struct
	type t = ltype VariableMap.t * kind VariableMap.t

	let empty = VariableMap.empty, VariableMap.empty

	let add_var v t (ts, ks) =
		VariableMap.add v t ts, ks

	let find_var v (ts, _) = VariableMap.find v ts

	let fold_vars f u (ts, _) = VariableMap.fold f ts u

	let add_kind t k (ts, ks) =
		ts, VariableMap.add t k ks

	let find_kind t (_, ks) = VariableMap.find t ks
end

let verbose = true

let em = ConstraintSet.empty
let kem = KindConstraintSet.empty

exception TypeError of string

type type_cs = Type of ltype * ConstraintSet.t * KindConstraintSet.t

let rec free_variables (ty : ltype) : VariableSet.t = match ty with
	| Typevar t' -> VariableSet.singleton t'
	| TInt | TBool | TUnit -> VariableSet.empty
	| TProduct(t1, t2)
	| TSum(t1, t2)
	| TFunction(t1, t2) -> VariableSet.union (free_variables t1) (free_variables t2)
	| TRef t' -> free_variables t'
	| TRecord lst -> List.fold_left VariableSet.union VariableSet.empty (List.map (fun (_, t) -> free_variables t) lst)
	| TForAll(lst, t') -> List.fold_left (fun x y -> VariableSet.remove y x) (free_variables t') lst
	| TADT(lst) ->
		let mapf (_, t) = List.fold_left VariableSet.union VariableSet.empty (List.map free_variables t) in
		List.fold_left VariableSet.union VariableSet.empty (List.map mapf lst)
	| TParameterized(t1, t2) -> VariableSet.union (free_variables t1) (free_variables t2)

let free_variables_context (c : Context.t) =
	Context.fold_vars (fun _ t a -> VariableSet.union (free_variables t) a) VariableSet.empty c

let rec replace_in_type typevar new_type t =
	match t with
	| Typevar t' when t' = typevar -> new_type
	| Typevar _ | TInt | TBool | TUnit -> t
	| TFunction(t1, t2) -> TFunction(replace_in_type typevar new_type t1, replace_in_type typevar new_type t2)
	| TProduct(t1, t2) -> TProduct(replace_in_type typevar new_type t1, replace_in_type typevar new_type t2)
	| TSum(t1, t2) -> TSum(replace_in_type typevar new_type t1, replace_in_type typevar new_type t2)
	| TRef t -> TRef(replace_in_type typevar new_type t)
	| TRecord lst -> TRecord(List.map (fun (l, t) -> l, replace_in_type typevar new_type t) lst)
	| TForAll(lst, t') -> if List.mem typevar lst then t else TForAll(lst, replace_in_type typevar new_type t')
	| TADT(lst) ->
		let mapf (l, t) = l, List.map (replace_in_type typevar new_type) t in
		TADT(List.map mapf lst)
	| TParameterized(t1, t2) -> TParameterized(replace_in_type typevar new_type t1, replace_in_type typevar new_type t2)

let rec replace_in_kind (kv : string) (new_kind : kind) (k : kind) = match k with
	| KVar x when kv = x -> new_kind
	| KVar _ | KStar -> k
	| KArrow(k1, k2) -> KArrow(replace_in_kind kv new_kind k1, replace_in_kind kv new_kind k2)

let quantify (ctxt : Context.t) (t : ltype) : ltype =
	let vars = VariableSet.diff (free_variables t) (free_variables_context ctxt) in
	if VariableSet.is_empty vars then t else TForAll(VariableSet.elements vars, t)

let instantiate t = match t with
	| TForAll(lst, t') -> List.fold_left (fun t tv -> replace_in_type tv (new_typevar()) t) t' lst
	| _ -> t

exception ImpossibleConstraint of string

type substitution = ltype VariableMap.t
type ksubstitution = kind VariableMap.t

let set_map f s = ConstraintSet.fold (fun elt s -> ConstraintSet.add (f elt) s) s em

let replace_type typevar new_type =
	let mapf t = match t with
		| Equals(t1, t2) ->
			Equals(replace_in_type typevar new_type t1, replace_in_type typevar new_type t2)
		| HasLabel(t1, l, t2) ->
			HasLabel(replace_in_type typevar new_type t1, l, replace_in_type typevar new_type t2) in
	set_map mapf

let replace_kind kvar new_kind =
	let mapf (KEquals(k1, k2)) =
		KEquals(replace_in_kind kvar new_kind k1, replace_in_kind kvar new_kind k2) in
	KindConstraintSet.fold (fun elt s -> KindConstraintSet.add (mapf elt) s) kem

let rec is_free_variable (t : string) (ty : ltype) = match ty with
	| Typevar t' -> t = t'
	| TInt | TBool | TUnit -> false
	| TProduct(t1, t2)
	| TSum(t1, t2)
	| TParameterized(t1, t2)
	| TFunction(t1, t2) -> is_free_variable t t1 || is_free_variable t t2
	| TRef t' -> is_free_variable t t'
	| TRecord lst -> List.exists (fun (l, t') -> is_free_variable t t') lst
	| TForAll(lst, t') -> not (List.mem t lst) && is_free_variable t t'
	| TADT(lst) -> List.exists (fun (_, t') -> List.exists (is_free_variable t) t') lst

let rec is_free_kvar (t : string) (k : kind) = match k with
	| KVar x -> t = x
	| KStar -> false
	| KArrow(k1, k2) -> is_free_kvar t k1 || is_free_kvar t k2

let rec unify (cs : ConstraintSet.t) : substitution =
	try (let chosen = ConstraintSet.choose cs in
		let new_set = ConstraintSet.remove chosen cs in
		match chosen with
		| Equals(t1, t2) when t1 = t2 -> unify new_set
		| Equals(t', Typevar t)
		| Equals(Typevar t, t') when (not(is_free_variable t t')) ->
			let new_cs = replace_type t t' new_set in
			let rest = unify new_cs in
			VariableMap.add t t' rest
		| Equals(TProduct(t0, t1), TProduct(t0', t1'))
		| Equals(TSum(t0, t1), TSum(t0', t1'))
		| Equals(TParameterized(t0, t1), TParameterized(t0', t1'))
		| Equals(TFunction(t0, t1), TFunction(t0', t1')) ->
			let new_cs = ConstraintSet.add (Equals(t0, t0')) new_set in
			let new_cs = ConstraintSet.add (Equals(t1, t1')) new_cs in
			unify new_cs
		| Equals(TRef t0, TRef t0') ->
			let new_cs = ConstraintSet.add (Equals(t0, t0')) new_set in
			unify new_cs
		| Equals(TForAll(_, _) as fa, t)
		| Equals(t, (TForAll(_, _) as fa)) ->
			let new_cs = ConstraintSet.add (Equals(t, instantiate fa)) new_set in
			unify new_cs
		| HasLabel(TRecord lst, l, t) ->
			let t' = try List.assoc l lst with Not_found ->
				let msg = "Label " ^ l ^ " does not exist in type: " ^ string_of_type (TRecord lst) in
				raise(ImpossibleConstraint msg) in
			let new_cs = ConstraintSet.add (Equals(t', t)) new_set in
			unify new_cs
		| HasLabel(t, l, t') -> raise(ImpossibleConstraint("Type is not a record type: " ^ string_of_type t))
		| Equals(t1, t2) ->
			let types = string_of_type t1 ^ " and " ^ string_of_type t2 in
			raise(ImpossibleConstraint("Cannot unify types: " ^ types)))
	with Not_found -> VariableMap.empty

let rec unify_kinds (ks : KindConstraintSet.t) : ksubstitution =
	try (let chosen = KindConstraintSet.choose ks in
		let ks = KindConstraintSet.remove chosen ks in
		match chosen with
		| KEquals(k1, k2) when k1 = k2 -> unify_kinds ks
		| KEquals(k', KVar k)
		| KEquals(KVar k, k') when not (is_free_kvar k k') ->
			let ks = replace_kind k k' ks in
			let rest = unify_kinds ks in
			VariableMap.add k k' rest
		| KEquals(KArrow(k1a, k1b), KArrow(k2a, k2b)) ->
			let ks = KindConstraintSet.add (KEquals(k1a, k1b)) ks in
			let ks = KindConstraintSet.add (KEquals(k2a, k2b)) ks in
			unify_kinds ks
		| KEquals(k1, k2) ->
			let kinds = string_of_kind k1 ^ " and " ^ string_of_kind k2 in
			raise(ImpossibleConstraint("Cannot unify kinds: " ^ kinds)))
	with Not_found -> VariableMap.empty

let rec apply_substitution (s : substitution) (t : ltype) (b : VariableSet.t) : ltype =
	match t with
	| TInt | TBool | TUnit -> t
	| TRef t' -> TRef(apply_substitution s t' b)
	| TFunction(t1, t2) -> TFunction(apply_substitution s t1 b, apply_substitution s t2 b)
	| TSum(t1, t2) -> TSum(apply_substitution s t1 b, apply_substitution s t2 b)
	| TProduct(t1, t2) -> TProduct(apply_substitution s t1 b, apply_substitution s t2 b)
	| Typevar tv when not (VariableSet.mem tv b) ->
		(try apply_substitution s (VariableMap.find tv s) b
		with Not_found -> t)
	| Typevar tv -> t
	| TRecord(lst) -> TRecord(List.map (fun (l, t) -> (l, apply_substitution s t b)) lst)
	| TForAll(lst, t') ->
		let new_set = List.fold_left (fun a e -> VariableSet.add e a) b lst in
		TForAll(lst, apply_substitution s t' new_set)
	| TADT lst -> TADT(List.map (fun (l, t) -> (l, List.map (fun t -> apply_substitution s t b) t)) lst)
	| TParameterized(t1, t2) -> TParameterized(apply_substitution s t1 b, apply_substitution s t2 b)

let rec get_kind (t : ltype) (c : Context.t) : kind * KindConstraintSet.t =
	let add = KindConstraintSet.add in
	let union = KindConstraintSet.union in
	match t with
	| TInt | TBool | TUnit -> KStar, kem
	| TProduct(t1, t2) | TSum(t1, t2) | TFunction(t1, t2) ->
		let k1, kc1 = get_kind t1 c in
		let k2, kc2 = get_kind t2 c in
		let kc = add (KEquals(k1, KStar)) (add (KEquals(k2, KStar)) (union kc1 kc2)) in
		KStar, kc
	| TRef t ->
		let k, kc = get_kind t c in
		let kc = add (KEquals(k, KStar)) kc in
		KStar, kc
	| Typevar tv ->
		(try Context.find_kind tv c, kem
		with Not_found -> raise(TypeError("Unbound type variable: " ^ tv)))
	| TRecord lst ->
		let foldf rest (_, t) =
			let k, kc = get_kind t c in
			add (KEquals(k, KStar)) (union kc rest) in
		let kc = List.fold_left foldf kem lst in
		KStar, kc
	| TForAll(lst, t') ->
		let foldf (c, vars) x =
			let kv = Ast.new_kindvar() in
			Context.add_kind x kv c, kv::vars in
		let new_c, vars = List.fold_left foldf (c, []) lst in
		let res, kc = get_kind t' new_c in
		let k = List.fold_left (fun rest kv -> KArrow(kv, rest)) res vars in
		k, kc
	| TADT lst ->
		let foldf rest (_, lst) =
			let inner_foldf rest t =
				let k1, kc1 = get_kind t c in
				add (KEquals(k1, KStar)) (union kc1 rest) in
			List.fold_left inner_foldf rest lst in
		let kc = List.fold_left foldf kem lst in
		KStar, kc
	| TParameterized(t1, t2) ->
		let k1, kc1 = get_kind t1 c in
		let k2, kc2 = get_kind t2 c in
		let result_k = Ast.new_kindvar() in
		result_k, add (KEquals(k1, KArrow(k2, result_k))) (union kc1 kc2)

(* Handle an optional type (that may or may not be given) plus its kind *)
let get_optional_type (t : ltype option) (ks : KindConstraintSet.t) (c : Context.t) : ltype * KindConstraintSet.t =
	match t with
	| None -> Ast.new_typevar(), ks
	| Some t' ->
		let k, ks' = get_kind t' c in
		let new_ks = KindConstraintSet.add (KEquals(k, KStar))
			(KindConstraintSet.union ks ks') in
		t', new_ks

let rec get_type (e : expr) (c : Context.t) : type_cs =
	match e with
	| Int _ -> Type(TInt, em, kem)
	| Bool _ -> Type(TBool, em, kem)
	| Unit -> Type(TUnit, em, kem)
	| Var x ->
		(try Type(instantiate (Context.find_var x c), em, kem)
			with Not_found -> raise (TypeError("Unbound variable: " ^ x)))
	| Let(x, t, e1, e2) ->
		let Type(t1, cs1, ks1) = get_type e1 c in
		let subst = unify cs1 in
		let t1' = apply_substitution subst t1 VariableSet.empty in
		let t1'' = quantify c t1' in
		let new_tc = Context.add_var x t1'' c in
		let Type(t2, cs2, ks2) = get_type e2 new_tc in
		let ks = KindConstraintSet.union ks1 ks2 in
		let t', ks' = get_optional_type t ks c in
		let new_cs = ConstraintSet.add (Equals(t', t1'')) (ConstraintSet.union cs1 cs2) in
		Type(t2, new_cs, ks')
	| LetRec(x, t, e1, e2) ->
		let t', ks' = get_optional_type t kem c in
		let e1_c = Context.add_var x t' c in
		let Type(t1, cs1, ks1) = get_type e1 e1_c in
		let subst = unify cs1 in
		let t1' = apply_substitution subst t1 VariableSet.empty in
		let t1'' = quantify e1_c t1' in
		let new_tc = Context.add_var x t1'' c in
		let Type(t2, cs2, ks2) = get_type e2 new_tc in
		let ks = KindConstraintSet.union ks' (KindConstraintSet.union ks1 ks2) in
		let new_cs = ConstraintSet.add (Equals(t', t1')) (ConstraintSet.union cs1 cs2) in
		Type(t2, new_cs, ks)
	| Binop(_, e1, e2) ->
		let Type(t1, cs1, ks1) = get_type e1 c in
		let Type(t2, cs2, ks2) = get_type e2 c in
		Type(TInt, ConstraintSet.add (Equals(t1, TInt))
			(ConstraintSet.add (Equals(t2, TInt))
				(ConstraintSet.union cs1 cs2)), KindConstraintSet.union ks1 ks2)
	| Boolbinop(_, e1, e2) ->
		let Type(t1, cs1, ks1) = get_type e1 c in
		let Type(t2, cs2, ks2) = get_type e2 c in
		Type(TBool, ConstraintSet.add (Equals(t1, TInt))
			(ConstraintSet.add (Equals(t2, TInt))
				(ConstraintSet.union cs1 cs2)), KindConstraintSet.union ks1 ks2)
	| Unop(_, e) ->
		let Type(t, cs, ks) = get_type e c in
		Type(TInt, ConstraintSet.add (Equals(t, TInt)) cs, ks)
	| Application(e1, e2) ->
		let Type(t1, cs1, ks1) = get_type e1 c in
		let Type(t2, cs2, ks2) = get_type e2 c in
		let new_type = Ast.new_typevar() in
		let new_cs = ConstraintSet.add (Equals(t1, TFunction(t2, new_type))) (ConstraintSet.union cs1 cs2) in
		Type(new_type, new_cs, KindConstraintSet.union ks1 ks2)
	| Abstraction(arg, t, body) ->
		let t', ks' = get_optional_type t kem c in
		let Type(t'', cs, ks) = get_type body (Context.add_var arg t' c) in
		Type(TFunction(t', t''), cs, KindConstraintSet.union ks ks')
	| Fix e ->
		let Type(t, cs, ks) = get_type e c in
		let new_type = Ast.new_typevar() in
		let new_cs = ConstraintSet.add (Equals(t, TFunction(new_type, new_type))) cs in
		Type(new_type, new_cs, ks)
	| If(e1, e2, e3) ->
		let Type(t1, cs1, ks1) = get_type e1 c in
		let Type(t2, cs2, ks2) = get_type e2 c in
		let Type(t3, cs3, ks3) = get_type e3 c in
		let new_cs = ConstraintSet.union cs1 (ConstraintSet.union cs2 cs3) in
		let new_cs = ConstraintSet.add (Equals(t1, TBool)) (ConstraintSet.add (Equals(t2, t3)) new_cs) in
		Type(t2, new_cs, KindConstraintSet.union ks1 (KindConstraintSet.union ks2 ks3))
	| Pair(e1, e2) ->
		let Type(t1, cs1, ks1) = get_type e1 c in
		let Type(t2, cs2, ks2) = get_type e2 c in
		Type(TProduct(t1, t2), ConstraintSet.union cs1 cs2, KindConstraintSet.union ks1 ks2)
	| Projection(b, e) ->
		let Type(t, cs, ks) = get_type e c in
		let left_typevar = Ast.new_typevar() in
		let right_typevar = Ast.new_typevar() in
		let new_constraint = Equals(t, TProduct(left_typevar, right_typevar)) in
		let my_type = if b then right_typevar else left_typevar in
		Type(my_type, ConstraintSet.add new_constraint cs, ks)
	| Injection(b, e) ->
		let Type(t, cs, ks) = get_type e c in
		let other_typevar = Ast.new_typevar() in
		let my_type = if b then TSum(other_typevar, t) else TSum(t, other_typevar) in
		Type(my_type, cs, ks)
	| Case(e1, e2, e3) ->
		let Type(t1, cs1, ks1) = get_type e1 c in
		let Type(t2, cs2, ks2) = get_type e2 c in
		let Type(t3, cs3, ks3) = get_type e3 c in
		let tv2a = Ast.new_typevar() in
		let tv3a = Ast.new_typevar() in
		let tv_res = Ast.new_typevar() in
		let new_cs = ConstraintSet.union cs1 (ConstraintSet.union cs2 cs3) in
		let with_sum = ConstraintSet.add (Equals(t1, TSum(tv2a, tv3a))) new_cs in
		let with_left = ConstraintSet.add (Equals(t2, TFunction(tv2a, tv_res))) with_sum in
		let with_right = ConstraintSet.add (Equals(t3, TFunction(tv3a, tv_res))) with_left in
		Type(tv_res, with_right, KindConstraintSet.union ks1 (KindConstraintSet.union ks2 ks3))
	| Allocation(e) ->
		let Type(t, cs, ks) = get_type e c in Type(TRef t, cs, ks)
	| Dereference(e) ->
		let Type(t, cs, ks) = get_type e c in
		let tv = Ast.new_typevar() in
		let new_cs = ConstraintSet.add (Equals(t, TRef tv)) cs in
		Type(tv, new_cs, ks)
	| Assignment(e1, e2) ->
		let Type(t1, cs1, ks1) = get_type e1 c in
		let Type(t2, cs2, ks2) = get_type e2 c in
		let new_cs = ConstraintSet.add (Equals(t1, TRef t2)) (ConstraintSet.union cs1 cs2) in
		Type(TUnit, new_cs, KindConstraintSet.union ks1 ks2)
	| Sequence(e1, e2) ->
		let Type(t1, cs1, ks1) = get_type e1 c in
		let Type(t2, cs2, ks2) = get_type e2 c in
		Type(t2, ConstraintSet.union cs1 cs2, KindConstraintSet.union ks1 ks2)
	| Reference e ->
		let Type(t, cs, ks) = get_type (!e) c in Type(TRef t, cs, ks)
 	| Record lst ->
		let foldf (rest, cs, ks) (l, e) =
			let Type(t, cs', ks') = get_type e c in
			(l, t)::rest, ConstraintSet.union cs cs', KindConstraintSet.union ks ks' in
		let t, cs, ks = List.fold_left foldf ([], em, kem) lst in
		Type(TRecord t, cs, ks)
	| Member(e, l) ->
		let Type(t, cs, ks) = get_type e c in
		let tv = Ast.new_typevar() in
		let new_cs = ConstraintSet.add (HasLabel(t, l, tv)) cs in
		Type(tv, new_cs, ks)
	| Constructor n -> (try Type(instantiate (Context.find_var n c), em, kem)
		with Not_found -> raise(TypeError("Unbound constructor " ^ n)))
	| LetType(name, params, lst, e) ->
		let result_t = List.fold_left (fun r p -> TParameterized(r, Typevar p)) (Typevar name) params in
		let kind = List.fold_left (fun r _ -> KArrow(KStar, r)) KStar params in
		let new_tc = Context.add_kind name kind c in
		let inner_tc = List.fold_left (fun r p -> Context.add_kind p (Ast.new_kindvar()) r) new_tc params in
		let foldf (rest, ks) (name, lst) =
			let inner_foldf (rest, ks) t =
				let k, ks' = get_kind t inner_tc in
				let new_ks = KindConstraintSet.add (KEquals(k, KStar)) (KindConstraintSet.union ks ks') in
				TFunction(t, rest), new_ks in
			let t, ks = List.fold_left inner_foldf (result_t, ks) lst in
			let t = if params = [] then t else TForAll(params, t) in
			Context.add_var name t rest, ks in
		let tc', new_ks = List.fold_left foldf (inner_tc, kem) lst in
		(* Include variables from the ADT definition in the new context, but not kind bindings *)
		let varc, _ = tc' in
		let _, kindc = new_tc in
		let Type(t1, cs1, ks1) = get_type e (varc, kindc) in
		Type(t1, cs1, KindConstraintSet.add (KEquals(KVar name, kind)) (KindConstraintSet.union new_ks ks1))
	| ADTInstance(_, _) -> raise(TypeError("Should not appear here"))
	| Match(e, lst) ->
		let Type(t1, cs1, ks1) = get_type e c in
		let res_tv = Ast.new_typevar() in
		let foldf (cs, ks) (p, e) =
			let rec type_pattern p t cs = match p with
			| PAnything -> ([], cs)
			| PVariable x -> ([(x, t)], cs)
			| PInt _ -> ([], ConstraintSet.add (Equals(t, TInt)) cs)
			| PBool _ -> ([], ConstraintSet.add (Equals(t, TBool)) cs)
			| PPair(p1, p2) ->
				let t1 = Ast.new_typevar() in
				let t2 = Ast.new_typevar() in
				let vars1, cs1 = type_pattern p1 t1 cs in
				let vars2, cs2 = type_pattern p2 t2 cs1 in
				(vars1 @ vars2, ConstraintSet.add (Equals(t, TProduct(t1, t2))) cs2)
			| PConstructor x ->
				let xt = (try instantiate (Context.find_var x c)
					with Not_found -> raise(TypeError("Unbound constructor " ^ x))) in
				([], ConstraintSet.add (Equals(xt, t)) cs)
			| PApplication(p1, p2) ->
				let t1 = Ast.new_typevar() in
				let t2 = Ast.new_typevar() in
				let vars1, cs1 = type_pattern p1 t1 cs in
				let vars2, cs2 = type_pattern p2 t2 cs1 in
				(vars1 @ vars2, ConstraintSet.add (Equals(t1, TFunction(t2, t))) cs2) in
			let vars, cs = type_pattern p t1 cs in
			let new_tc = List.fold_left (fun rest (x, t) -> Context.add_var x t rest) c vars in
			let Type(t', cs', ks') = get_type e new_tc in
			let new_cs = ConstraintSet.add (Equals(res_tv, t')) (ConstraintSet.union cs cs') in
			let new_ks = KindConstraintSet.union ks ks' in
			new_cs, new_ks in
		let cs, ks = List.fold_left foldf (cs1, ks1) lst in
		Type(res_tv, cs, ks)

let print_cs cs = ConstraintSet.fold (fun e a -> (match e with
	| Equals(t1, t2) -> "\t" ^ string_of_type t1 ^ " = " ^ string_of_type t2
	| HasLabel(t1, l, t2) -> "\t" ^ string_of_type t1 ^ " has label " ^ l ^ " of type " ^ string_of_type t2) ^ "\n" ^ a) cs ""

let print_ks ks = KindConstraintSet.fold (fun (KEquals(k1, k2)) a ->
	"\t" ^ string_of_kind k1 ^ " = " ^ string_of_kind k2 ^ "\n" ^ a) ks ""

let typecheck e =
	try let Type(t, cs, ks) = get_type e Context.empty in
		(try
			if verbose then (Printf.printf "Initial type: %s\n" (string_of_type t);
				Printf.printf "Constraints:\n%s\n" (print_cs cs);
				Printf.printf "Kind constraints:\n%s\n" (print_ks ks));
			let _ = unify_kinds ks in
			let subst = unify cs in
			if verbose then
				let res_t = apply_substitution subst t VariableSet.empty in
				Printf.printf "Final type: %s\n" (string_of_type res_t)
			else ignore subst;
			None
		with ImpossibleConstraint e -> Some e)
	with TypeError e -> Some e
