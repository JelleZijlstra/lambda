open Ast

type errmsg = string

type typecheck_t = TError of errmsg | Result of Ast.expr

type lconstraint = Equals of ltype * ltype | HasLabel of ltype * string * ltype

type kconstraint = KEquals of kind * kind

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
	type t = ltype VarMap.t * kind VarMap.t * ltype VarMap.t * string

	let empty loc = VarMap.empty, VarMap.empty, VarMap.empty, loc

	let add_var v t (ts, ks, tss, l) =
		VarMap.add v t ts, ks, tss, l

	let find_var v (ts, _, _, _) = VarMap.find v ts

	let fold_vars f u (ts, _, _, _) = VarMap.fold f ts u

	let add_kind t k (ts, ks, vss, l) =
		ts, VarMap.add t k ks, vss, l

	let find_kind t (_, ks, _, _) = VarMap.find t ks

	let add_type_synonym x t (ts, ks, vss, l) =
		ts, ks, VarMap.add x t vss, l

	let find_type_synonym x (_, _, vss, _) =
		try Some(VarMap.find x vss) with Not_found -> None

	let get_loc (_, _, _, l) = l

	let set_loc l (ts, ks, vss, _) = (ts, ks, vss, l)
end

let em = ConstraintSet.empty
let kem = KindConstraintSet.empty

exception TypeError of string

type type_cs = Type of ltype * expr * ConstraintSet.t * KindConstraintSet.t

let rec free_variables (ty : ltype) : VariableSet.t = match ty with
	| Typevar t' -> VariableSet.singleton t'
	| TInt | TBool | TUnit | TString -> VariableSet.empty
	| TProduct(t1, t2)
	| TSum(t1, t2)
	| TParameterized(t1, t2)
	| TFunction(t1, t2) -> VariableSet.union (free_variables t1) (free_variables t2)
	| TRef t' -> free_variables t'
	| TypeWithLabel(_, lst) ->
		List.fold_left VariableSet.union VariableSet.empty (List.map (fun (_, t) -> free_variables t) lst)
	| TRecord lst ->
		let types = VarMap.fold (fun _ t rest -> free_variables t::rest) lst [] in
		List.fold_left VariableSet.union VariableSet.empty types
	| TForAll(lst, t') -> List.fold_left (fun x y -> VariableSet.remove y x) (free_variables t') lst
	| TADT(lst) ->
		let mapf (_, t) = List.fold_left VariableSet.union VariableSet.empty (List.map free_variables t) in
		List.fold_left VariableSet.union VariableSet.empty (List.map mapf lst)
	| TModule lst ->
		let foldf entry rest = match entry with
			| AbstractType(n, _) -> VariableSet.remove n rest
			| ConcreteType(n, params, t) ->
				let t_vars = List.fold_left (fun x y -> VariableSet.remove y x) (free_variables t) params in
				VariableSet.remove n (VariableSet.union t_vars rest)
			| Value(n, t) -> VariableSet.union (free_variables t) rest in
		List.fold_right foldf lst VariableSet.empty

let free_variables_context (c : Context.t) =
	Context.fold_vars (fun _ t a -> VariableSet.union (free_variables t) a) VariableSet.empty c

let rec replace_in_type typevar new_type t =
	match t with
	| Typevar t' when t' = typevar -> new_type
	| TypeWithLabel(n, lst) when n = typevar -> new_type
	| Typevar _ | TInt | TBool | TUnit | TString -> t
	| TFunction(t1, t2) -> TFunction(replace_in_type typevar new_type t1, replace_in_type typevar new_type t2)
	| TProduct(t1, t2) -> TProduct(replace_in_type typevar new_type t1, replace_in_type typevar new_type t2)
	| TSum(t1, t2) -> TSum(replace_in_type typevar new_type t1, replace_in_type typevar new_type t2)
	| TRef t -> TRef(replace_in_type typevar new_type t)
	| TRecord lst -> TRecord(VarMap.map (replace_in_type typevar new_type) lst)
	| TForAll(lst, t') -> if List.mem typevar lst then t else TForAll(lst, replace_in_type typevar new_type t')
	| TADT(lst) ->
		let mapf (l, t) = l, List.map (replace_in_type typevar new_type) t in
		TADT(List.map mapf lst)
	| TParameterized(t1, t2) -> TParameterized(replace_in_type typevar new_type t1, replace_in_type typevar new_type t2)
	| TypeWithLabel(n, lst) ->
		let mapf (l, t) = l, replace_in_type typevar new_type t in
		TypeWithLabel(n, List.map mapf lst)
	| TModule lst ->
		let rec loop lst = match lst with
			| [] -> []
			| ((AbstractType(n, _)) | (ConcreteType(n, _, _)))::tl when n = typevar -> lst
			| ConcreteType(_, params, _)::tl when List.mem typevar params -> lst
			| ConcreteType(n, params, t)::tl ->
				ConcreteType(n, params, replace_in_type typevar new_type t)::loop tl
			| AbstractType(n, params)::tl -> AbstractType(n, params)::loop tl
			| Value(n, t)::tl -> Value(n, replace_in_type typevar new_type t)::loop tl in
		TModule(loop lst)

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

(* Select the more specialized type (useful for module typing at check time) *)
let rec select_type t1 t2 = match t1, t2 with
	| Typevar _, _ -> t2
	| _, Typevar _ -> t1
	| TFunction(t1a, t1b), TFunction(t2a, t2b) -> TFunction(select_type t1a t1b, select_type t2a t2b)
	| TProduct(t1a, t1b), TProduct(t2a, t2b) -> TProduct(select_type t1a t1b, select_type t2a t2b)
	| TSum(t1a, t1b), TSum(t2a, t2b) -> TSum(select_type t1a t1b, select_type t2a t2b)
	| _ -> t1 (* Not worth it for the rest *)

let mapify_module_type (actual : module_type_entry list) : module_type_entry VarMap.t * ltype VarMap.t =
	let foldf (tmap, vmap) entry = match entry with
		| ConcreteType(n, _, _) | AbstractType(n, _) ->
			(VarMap.add n entry tmap, vmap)
		| Value(x, t) ->
			(tmap, VarMap.add x t vmap)
	in
	List.fold_left foldf (VarMap.empty, VarMap.empty) actual

exception ImpossibleConstraint of string

type substitution = ltype VarMap.t
type ksubstitution = kind VarMap.t

let print_cs cs = ConstraintSet.fold (fun e a -> (match e with
	| Equals(t1, t2) -> "\t" ^ string_of_type t1 ^ " = " ^ string_of_type t2
	| HasLabel(t1, l, t2) -> "\t" ^ string_of_type t1 ^ " has label " ^ l ^ " of type " ^ string_of_type t2) ^ "\n" ^ a) cs ""

let print_ks ks = KindConstraintSet.fold (fun (KEquals(k1, k2)) a ->
	"\t" ^ string_of_kind k1 ^ " = " ^ string_of_kind k2 ^ "\n" ^ a) ks ""

let print_substitution s =
	let foldf x t rest = "\t" ^ x ^ " = " ^ string_of_type t ^ "\n" ^ rest in
	VarMap.fold foldf s ""

let print_ksubstitution s =
	let foldf x t rest = "\t" ^ x ^ " = " ^ string_of_kind t ^ "\n" ^ rest in
	VarMap.fold foldf s ""

let set_map f s = ConstraintSet.fold (fun elt s -> ConstraintSet.add (f elt) s) s em

let replace_type typevar new_type =
	let mapf t = match t with
		| Equals(t1, t2) ->
			Equals(replace_in_type typevar new_type t1, replace_in_type typevar new_type t2)
		| HasLabel(t1, l, t2) ->
			HasLabel(replace_in_type typevar new_type t1, l, replace_in_type typevar new_type t2) in
	set_map mapf

let replace_kind kvar new_kind ks =
	let mapf (KEquals(k1, k2)) =
		KEquals(replace_in_kind kvar new_kind k1, replace_in_kind kvar new_kind k2) in
	KindConstraintSet.fold (fun elt s -> KindConstraintSet.add (mapf elt) s) ks kem

let rec is_free_variable (t : string) (ty : ltype) = match ty with
	| Typevar t' -> t = t'
	| TInt | TBool | TUnit | TString -> false
	| TProduct(t1, t2)
	| TSum(t1, t2)
	| TParameterized(t1, t2)
	| TFunction(t1, t2) -> is_free_variable t t1 || is_free_variable t t2
	| TRef t' -> is_free_variable t t'
	| TRecord lst -> VarMap.exists (fun _ t' -> is_free_variable t t') lst
	| TForAll(lst, t') -> not (List.mem t lst) && is_free_variable t t'
	| TADT(lst) -> List.exists (fun (_, t') -> List.exists (is_free_variable t) t') lst
	| TypeWithLabel(n, lst) -> n = t || List.exists (fun (_, t') -> is_free_variable t t') lst
	| TModule lst ->
		let rec loop lst = match lst with
			| [] -> false
			| Value(n, t')::tl -> is_free_variable t t' || loop tl
			| (ConcreteType(n, _, _) | AbstractType(n, _))::_ when n = t -> false
			| ConcreteType(_, params, _)::_ when List.mem t params -> false
			| ConcreteType(_, _, t')::tl -> is_free_variable t t' || loop tl
			| AbstractType(_, _)::tl -> loop tl in
		loop lst

let rec is_free_kvar (t : string) (k : kind) = match k with
	| KVar x -> t = x
	| KStar -> false
	| KArrow(k1, k2) -> is_free_kvar t k1 || is_free_kvar t k2

let add_label (x : string) (label : string) (t : ltype) (s : substitution) : substitution =
	try match VarMap.find x s with
		| TypeWithLabel(x', lst) when x = x' ->
			VarMap.add x (TypeWithLabel(x', (label, t)::lst)) s
		| _ -> s
	with
	| Not_found -> VarMap.add x (TypeWithLabel(x, [(label, t)])) s

let rec unify (cs : ConstraintSet.t) : substitution =
	try (let chosen = ConstraintSet.choose cs in
		let new_set = ConstraintSet.remove chosen cs in
		match chosen with
		| Equals(TRecord l1, TRecord l2) when VarMap.equal (=) l1 l2 -> unify new_set
		| Equals(t1, t2) when t1 = t2 -> unify new_set
		| Equals(t', Typevar t)
		| Equals(Typevar t, t') when (not(is_free_variable t t')) ->
			let new_cs = replace_type t t' new_set in
			let rest = unify new_cs in
			VarMap.add t t' rest
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
		| Equals(TypeWithLabel(n1, lst1), TypeWithLabel(n2, lst2)) ->
			let new_type = TypeWithLabel(n1, lst1 @ lst2) in
			let new_cs = replace_type n2 new_type new_set in
			unify new_cs
		| Equals(TypeWithLabel(n, lst1) as t1, (TRecord lst2 as t2))
		| Equals(TRecord lst2 as t2, (TypeWithLabel(n, lst1) as t1)) ->
			(try
				let foldf rest (l, t) =
					let t' = VarMap.find l lst2 in
					ConstraintSet.add (Equals(t, t')) rest
				in
				let new_cs = List.fold_left foldf new_set lst1 in
				let result = unify new_cs in
				VarMap.add n t2 result
			with Not_found ->
				let types = string_of_type t1 ^ " and " ^ string_of_type t2 in
				raise(ImpossibleConstraint("Cannot unify types: " ^ types)))
		| Equals(TRecord lst1, TRecord lst2) when VarMap.cardinal lst1 = VarMap.cardinal lst2 ->
			let foldf l t1 rest =
				let t2 = VarMap.find l lst2 in
				ConstraintSet.add (Equals(t1, t2)) rest in
			let new_cs = VarMap.fold foldf lst1 new_set in
			unify new_cs
		| HasLabel(Typevar n, l, t') ->
			let new_type = TypeWithLabel(n, [l, t']) in
			let new_cs = replace_type n new_type new_set in
			let result = unify new_cs in
			add_label n l t' result
		| HasLabel(TypeWithLabel(n, lst), l, t') ->
			let new_type = TypeWithLabel(n, (l, t')::lst) in
			let new_cs = replace_type n new_type new_set in
			let result = unify new_cs in
			add_label n l t' result
		| HasLabel(TRecord lst, l, t) ->
			let t' = try VarMap.find l lst with Not_found ->
				let msg = "Label " ^ l ^ " does not exist in type: " ^ string_of_type (TRecord lst) in
				raise(ImpossibleConstraint msg) in
			let new_cs = ConstraintSet.add (Equals(t', t)) new_set in
			unify new_cs
		| HasLabel(t, l, t') -> raise(ImpossibleConstraint("Type is not a record type: " ^ string_of_type t))
(* 		| Equals(TModule t1, TModule t2) ->
 *)
		| Equals(t1, t2) ->
			let types = string_of_type t1 ^ " and " ^ string_of_type t2 in
			raise(ImpossibleConstraint("Cannot unify types: " ^ types)))
	with Not_found -> VarMap.empty

let rec unify_kinds (ks : KindConstraintSet.t) : ksubstitution =
	try (let chosen = KindConstraintSet.choose ks in
		let ks = KindConstraintSet.remove chosen ks in
		match chosen with
		| KEquals(k1, k2) when k1 = k2 -> unify_kinds ks
		| KEquals(k', KVar k)
		| KEquals(KVar k, k') when not (is_free_kvar k k') ->
			let ks = replace_kind k k' ks in
			let rest = unify_kinds ks in
			VarMap.add k k' rest
		| KEquals(KArrow(k1a, k1b), KArrow(k2a, k2b)) ->
			let ks = KindConstraintSet.add (KEquals(k1a, k1b)) ks in
			let ks = KindConstraintSet.add (KEquals(k2a, k2b)) ks in
			unify_kinds ks
		| KEquals(k1, k2) ->
			let kinds = string_of_kind k1 ^ " and " ^ string_of_kind k2 in
			raise(ImpossibleConstraint("Cannot unify kinds: " ^ kinds)))
	with Not_found -> VarMap.empty

let rec apply_substitution (s : substitution) (t : ltype) (b : VariableSet.t) : ltype =
	match t with
	| TInt | TBool | TUnit | TString -> t
	| TRef t' -> TRef(apply_substitution s t' b)
	| TFunction(t1, t2) -> TFunction(apply_substitution s t1 b, apply_substitution s t2 b)
	| TSum(t1, t2) -> TSum(apply_substitution s t1 b, apply_substitution s t2 b)
	| TProduct(t1, t2) -> TProduct(apply_substitution s t1 b, apply_substitution s t2 b)
	| Typevar tv when not (VariableSet.mem tv b) ->
		(try apply_substitution s (VarMap.find tv s) b
		with Not_found -> t)
	| Typevar tv -> t
	| TRecord(lst) -> TRecord(VarMap.map (fun t -> apply_substitution s t b) lst)
	| TForAll(lst, t') ->
		let new_set = List.fold_left (fun a e -> VariableSet.add e a) b lst in
		TForAll(lst, apply_substitution s t' new_set)
	| TADT lst -> TADT(List.map (fun (l, t) -> (l, List.map (fun t -> apply_substitution s t b) t)) lst)
	| TParameterized(t1, t2) -> TParameterized(apply_substitution s t1 b, apply_substitution s t2 b)
	| TypeWithLabel(name, lst) ->
		TypeWithLabel(name, List.map (fun (l, t) -> l, apply_substitution s t b) lst)
	| TModule lst ->
		let rec loop lst b = match lst with
			| [] -> []
			| AbstractType(n, params)::tl -> AbstractType(n, params)::loop tl (VariableSet.add n b)
			| ConcreteType(n, params, t)::tl ->
				let new_b = VariableSet.add n b in
				ConcreteType(n, params, apply_substitution s t new_b)::loop tl new_b
			| Value(n, t)::tl -> Value(n, apply_substitution s t b)::loop tl b in
		TModule(loop lst b)

let rec get_kind (t : ltype) (c : Context.t) : ltype * kind * KindConstraintSet.t =
	let add = KindConstraintSet.add in
	let union = KindConstraintSet.union in
	let type_binary t1 t2 =
		let t1, k1, kc1 = get_kind t1 c in
		let t2, k2, kc2 = get_kind t2 c in
		let kc = add (KEquals(k1, KStar)) (add (KEquals(k2, KStar)) (union kc1 kc2)) in
		t1, t2, kc in
	match t with
	| TInt | TBool | TUnit | TString -> t, KStar, kem
	| TProduct(t1, t2) -> let t1, t2, kc = type_binary t1 t2 in TProduct(t1, t2), KStar, kc
	| TSum(t1, t2) -> let t1, t2, kc = type_binary t1 t2 in TSum(t1, t2), KStar, kc
	| TFunction(t1, t2) -> let t1, t2, kc = type_binary t1 t2 in TFunction(t1, t2), KStar, kc
	| TRef t ->
		let t, k, kc = get_kind t c in
		let kc = add (KEquals(k, KStar)) kc in
		TRef t, KStar, kc
	| Typevar tv ->
		(try
			let k = Context.find_kind tv c in
			let t = match Context.find_type_synonym tv c with
				| Some t' -> t'
				| None -> Typevar tv in
			t, k, kem
		with Not_found -> raise(TypeError("Unbound type variable: " ^ tv)))
	| TypeWithLabel(n, lst) ->
		let foldf (l, t) (lst, rest) =
			let t, k, kc = get_kind t c in
			(l, t)::lst, add (KEquals(k, KStar)) (union kc rest) in
		let lst, kc = List.fold_right foldf lst ([], kem) in
		TypeWithLabel(n, lst), KStar, kc
	| TRecord lst ->
		let foldf l t (map, rest) =
			let t, k, kc = get_kind t c in
			VarMap.add l t map, add (KEquals(k, KStar)) (union kc rest) in
		let map, kc = VarMap.fold foldf lst (VarMap.empty, kem) in
		TRecord map, KStar, kc
	| TForAll(lst, t') ->
		let foldf (c, vars) x =
			let kv = Ast.new_kindvar() in
			Context.add_kind x kv c, kv::vars in
		let new_c, vars = List.fold_left foldf (c, []) lst in
		let t', res, kc = get_kind t' new_c in
		let k = List.fold_left (fun rest kv -> KArrow(kv, rest)) res vars in
		TForAll(lst, t'), k, kc
	| TADT lst ->
		let foldf (cons, lst) (t, rest) =
			let inner_foldf t (params, rest) =
				let t1, k1, kc1 = get_kind t c in
				t1::params, add (KEquals(k1, KStar)) (union kc1 rest) in
			let params, kc = List.fold_right inner_foldf lst ([], rest) in
			((cons, params)::t, kc) in
		let lst', kc = List.fold_right foldf lst ([], kem) in
		TADT lst', KStar, kc
	| TParameterized(t1, t2) ->
		let t1', k1, kc1 = get_kind t1 c in
		let t2', k2, kc2 = get_kind t2 c in
		let result_k = Ast.new_kindvar() in
		TParameterized(t1', t2'), result_k, add (KEquals(k1, KArrow(k2, result_k))) (union kc1 kc2)
	| TModule lst ->
 		let rec loop lst c = match lst with
			| [] -> ([], kem)
			| Value(n, t)::tl ->
				let t', k, kc = get_kind t c in
				let lst, rest = loop tl c in
				Value(n, t')::lst, union kc rest
			| ConcreteType(n, lst, TADT t)::tl ->
				let kc, c' = type_adt n lst t c in
				let lst', rest = loop tl c' in
				ConcreteType(n, lst, TADT t)::lst', union kc rest
			| ConcreteType(_, _, _)::_ ->
				raise(TypeError("Concrete module type must be ADT for now"))
			| AbstractType(n, lst)::tl ->
				let k = List.fold_left (fun rest _ -> KArrow(KStar, rest)) KStar lst in
				let c' = Context.add_kind n k c in
				let lst', rest = loop tl c' in
				AbstractType(n, lst)::lst', rest in
		let lst', kc = loop lst c in
		TModule lst', KStar, kc

and type_adt (name : string) (params : string list) (lst : adt) (c : Context.t) : KindConstraintSet.t * Context.t =
	let qualified_name = qualify_name name in
	let result_t = List.fold_left (fun r p -> TParameterized(r, Typevar p)) (Typevar qualified_name) params in
	let foldf (k, tc) p =
		let kv = KVar(qualify_name p) in
		(KArrow(kv, k), Context.add_kind p kv tc) in
	let kind, tc = List.fold_left foldf (KStar, c) params in
	let new_tc = Context.add_kind name kind tc in
	let foldf (rest, ks) (name, lst) =
		let inner_foldf (rest, ks) t =
			let _, k, ks' = get_kind t new_tc in
			let new_ks = KindConstraintSet.add (KEquals(k, KStar)) (KindConstraintSet.union ks ks') in
			TFunction(t, rest), new_ks in
		let t, ks = List.fold_left inner_foldf (result_t, ks) lst in
		let t = if params = [] then t else TForAll(params, t) in
		Context.add_var name t rest, ks in
	let tc', new_ks = List.fold_left foldf (tc, kem) lst in
	(* Include variables from the ADT definition in the new context, but not kind bindings *)
	let varc, _, _, _ = tc' in
	let _, kindc, tss, _ = new_tc in
	let new_ks = KindConstraintSet.add (KEquals(KVar qualified_name, kind)) new_ks in
	(new_ks, (varc, kindc, tss, Context.get_loc c))


(* Handle an optional type (that may or may not be given) plus its kind *)
let get_optional_type (t : ltype option) (ks : KindConstraintSet.t) (c : Context.t) : ltype * KindConstraintSet.t =
	match t with
	| None -> Ast.new_typevar(), ks
	| Some t' ->
		let t'', k, ks' = get_kind t' c in
		let new_ks = KindConstraintSet.add (KEquals(k, KStar))
			(KindConstraintSet.union ks ks') in
		t'', new_ks

let check_module_type (interface : module_type_entry list) (actual : module_type_entry list) =
	(* First, create two maps from the actual type, to enable easy lookup
		while looping over the interface. *)
	let tmap, vmap = mapify_module_type actual in

	let check_foldf (t, cs) entry = try
		match entry with
		| ConcreteType(n, params, tp) -> (match VarMap.find n tmap with
			| ConcreteType(n', params', tp') when n = n' && params = params' && tp = tp' ->
				(entry::t, cs)
			| ConcreteType(_, _, _) as e ->
				let msg = "Interface and implementation do not match: " ^ string_of_module_type_entry entry ^ " and " ^ string_of_module_type_entry e in
				raise(TypeError msg)
			| _ -> failwith "impossible")
		| AbstractType(n, params) -> (match VarMap.find n tmap with
			| ConcreteType(n', params', _) when n = n' && params = params' ->
				(entry::t, cs)
			| ConcreteType(_, _, _) as e ->
				let msg = "Interface and implementation do not match: " ^ string_of_module_type_entry entry ^ " and " ^ string_of_module_type_entry e in
				raise(TypeError msg)
			| _ -> failwith "impossible")
		| Value(x, tp) ->
			let tp' = VarMap.find x vmap in
			(entry::t, ConstraintSet.add (Equals(tp, tp')) cs)
	with Not_found ->
		let text = string_of_module_type_entry entry in
		let msg = "Module types do not match, cannot find interface member " ^ text ^ " in implementation" in
		raise(TypeError msg)
	in
	List.fold_left check_foldf ([], em) interface

let rec get_type (e : expr) (c : Context.t) : type_cs =
	match e with
	| Int _ -> Type(TInt, e, em, kem)
	| Bool _ -> Type(TBool, e, em, kem)
	| Unit -> Type(TUnit, e, em, kem)
	| String _ -> Type(TString, e, em, kem)
	| Var x ->
		(try Type(instantiate (Context.find_var x c), e, em, kem)
			with Not_found -> raise (TypeError("Unbound variable: " ^ x)))
	| In(Let(x, t, e1), e2) ->
		let tc, t', e1', cs1, ks1 = get_type_let e1 x t c in
		let Type(t2, e2', cs2, ks2) = get_type e2 tc in
		Type(t2, In(Let(x, Some t', e1'), e2'), ConstraintSet.union cs1 cs2, KindConstraintSet.union ks1 ks2)
	| In(LetRec(x, t, e1), e2) ->
		let tc, t', e1', cs1, ks1 = get_type_let_rec e1 x t c in
		let Type(t2, e2', cs2, ks2) = get_type e2 tc in
		Type(t2, In(LetRec(x, Some t', e1'), e2'), ConstraintSet.union cs1 cs2, KindConstraintSet.union ks1 ks2)
	| Binop(op, e1, e2) ->
		let Type(t1, e1, cs1, ks1) = get_type e1 c in
		let Type(t2, e2, cs2, ks2) = get_type e2 c in
		Type(TInt, Binop(op, e1, e2),
			ConstraintSet.add (Equals(t1, TInt))
			(ConstraintSet.add (Equals(t2, TInt))
				(ConstraintSet.union cs1 cs2)), KindConstraintSet.union ks1 ks2)
	| Boolbinop(op, e1, e2) ->
		let Type(t1, e1, cs1, ks1) = get_type e1 c in
		let Type(t2, e2, cs2, ks2) = get_type e2 c in
		Type(TBool, Boolbinop(op, e1, e2),
			ConstraintSet.add (Equals(t1, TInt))
			(ConstraintSet.add (Equals(t2, TInt))
				(ConstraintSet.union cs1 cs2)), KindConstraintSet.union ks1 ks2)
	| Unop(op, e) ->
		let Type(t, e, cs, ks) = get_type e c in
		Type(TInt, Unop(op, e), ConstraintSet.add (Equals(t, TInt)) cs, ks)
	| Application(e1, e2) ->
		let Type(t1, e1, cs1, ks1) = get_type e1 c in
		let Type(t2, e2, cs2, ks2) = get_type e2 c in
		let new_type = Ast.new_typevar() in
		let new_cs = ConstraintSet.add (Equals(t1, TFunction(t2, new_type))) (ConstraintSet.union cs1 cs2) in
		Type(new_type, Application(e1, e2), new_cs, KindConstraintSet.union ks1 ks2)
	| Abstraction(arg, t, body) ->
		let t', ks' = get_optional_type t kem c in
		let Type(t'', body, cs, ks) = get_type body (Context.add_var arg t' c) in
		Type(TFunction(t', t''), Abstraction(arg, Some t', body), cs, KindConstraintSet.union ks ks')
	| Fix e ->
		let Type(t, e, cs, ks) = get_type e c in
		let new_type = Ast.new_typevar() in
		let new_cs = ConstraintSet.add (Equals(t, TFunction(new_type, new_type))) cs in
		Type(new_type, Fix e, new_cs, ks)
	| If(e1, e2, e3) ->
		let Type(t1, e1, cs1, ks1) = get_type e1 c in
		let Type(t2, e2, cs2, ks2) = get_type e2 c in
		let Type(t3, e3, cs3, ks3) = get_type e3 c in
		let new_cs = ConstraintSet.union cs1 (ConstraintSet.union cs2 cs3) in
		let new_cs = ConstraintSet.add (Equals(t1, TBool)) (ConstraintSet.add (Equals(t2, t3)) new_cs) in
		Type(t2, If(e1, e2, e3), new_cs, KindConstraintSet.union ks1 (KindConstraintSet.union ks2 ks3))
	| Pair(e1, e2) ->
		let Type(t1, e1, cs1, ks1) = get_type e1 c in
		let Type(t2, e2, cs2, ks2) = get_type e2 c in
		Type(TProduct(t1, t2), Pair(e1, e2), ConstraintSet.union cs1 cs2, KindConstraintSet.union ks1 ks2)
	| Projection(b, e) ->
		let Type(t, e, cs, ks) = get_type e c in
		let left_typevar = Ast.new_typevar() in
		let right_typevar = Ast.new_typevar() in
		let new_constraint = Equals(t, TProduct(left_typevar, right_typevar)) in
		let my_type = if b then right_typevar else left_typevar in
		Type(my_type, Projection(b, e), ConstraintSet.add new_constraint cs, ks)
	| Injection(b, e) ->
		let Type(t, e, cs, ks) = get_type e c in
		let other_typevar = Ast.new_typevar() in
		let my_type = if b then TSum(other_typevar, t) else TSum(t, other_typevar) in
		Type(my_type, Injection(b, e), cs, ks)
	| Case(e1, e2, e3) ->
		let Type(t1, e1, cs1, ks1) = get_type e1 c in
		let Type(t2, e2, cs2, ks2) = get_type e2 c in
		let Type(t3, e3, cs3, ks3) = get_type e3 c in
		let tv2a = Ast.new_typevar() in
		let tv3a = Ast.new_typevar() in
		let tv_res = Ast.new_typevar() in
		let new_cs = ConstraintSet.union cs1 (ConstraintSet.union cs2 cs3) in
		let with_sum = ConstraintSet.add (Equals(t1, TSum(tv2a, tv3a))) new_cs in
		let with_left = ConstraintSet.add (Equals(t2, TFunction(tv2a, tv_res))) with_sum in
		let with_right = ConstraintSet.add (Equals(t3, TFunction(tv3a, tv_res))) with_left in
		Type(tv_res, Case(e1, e2, e3), with_right, KindConstraintSet.union ks1 (KindConstraintSet.union ks2 ks3))
	| Allocation(e) ->
		let Type(t, e, cs, ks) = get_type e c in Type(TRef t, Allocation e, cs, ks)
	| Dereference(e) ->
		let Type(t, e, cs, ks) = get_type e c in
		let tv = Ast.new_typevar() in
		let new_cs = ConstraintSet.add (Equals(t, TRef tv)) cs in
		Type(tv, Dereference e, new_cs, ks)
	| Assignment(e1, e2) ->
		let Type(t1, e1, cs1, ks1) = get_type e1 c in
		let Type(t2, e2, cs2, ks2) = get_type e2 c in
		let new_cs = ConstraintSet.add (Equals(t1, TRef t2)) (ConstraintSet.union cs1 cs2) in
		Type(TUnit, Assignment(e1, e2), new_cs, KindConstraintSet.union ks1 ks2)
	| Sequence(e1, e2) ->
		let Type(t1, e1, cs1, ks1) = get_type e1 c in
		let Type(t2, e2, cs2, ks2) = get_type e2 c in
		Type(t2, Sequence(e1, e2), ConstraintSet.union cs1 cs2, KindConstraintSet.union ks1 ks2)
 	| Record lst ->
		let foldf l e (rest, record_e, cs, ks) =
			let Type(t, e, cs', ks') = get_type e c in
			VarMap.add l t rest, VarMap.add l e record_e, ConstraintSet.union cs cs', KindConstraintSet.union ks ks' in
		let t, lst, cs, ks = VarMap.fold foldf lst (VarMap.empty, VarMap.empty, em, kem) in
		Type(TRecord t, Record lst, cs, ks)
	| Member(e, l) ->
		let Type(t, e, cs, ks) = get_type e c in
		(match t with
			| TModule lst ->
				(* Only if the e is immediately resolvable as a module do we interpret this as a module access. *)
				(* TODO: Change TModule into a map too. *)
				let rec find_var lst = match lst with
					| [] -> raise(TypeError("Unbound module member " ^ l ^ " in " ^ string_of_type t))
					| Value(n, t')::_ when n = l -> t'
					| ConcreteType(n, params, TADT t)::tl ->
						let _, c = type_adt n params t (Context.empty (Context.get_loc c)) in
						(try Context.find_var l c with Not_found -> find_var tl)
					| _::tl -> find_var tl in
				Type(find_var lst, Member(e, l), cs, ks)
			| _ ->
				let tv = Ast.new_typevar() in
				let new_cs = ConstraintSet.add (HasLabel(t, l, tv)) cs in
				Type(tv, Member(e, l), new_cs, ks))
	| ConstructorMember(e, l) ->
		let Type(t, e, cs, ks) = get_type e c in
		(match t with
			| TModule lst ->
				let rec find_var lst = match lst with
					| [] -> raise(TypeError("Unbound module member " ^ l ^ " in " ^ string_of_type t))
					| ConcreteType(n, params, TADT t)::tl ->
						let _, c = type_adt n params t (Context.empty (Context.get_loc c)) in
						(try Context.find_var l c with Not_found -> find_var tl)
					| _::tl -> find_var tl in
				Type(find_var lst, ConstructorMember(e, l), cs, ks)
			| _ -> raise(TypeError("Constructors can only appear in modules")))
	| Constructor n -> (try Type(instantiate (Context.find_var n c), e, em, kem)
		with Not_found -> raise(TypeError("Unbound constructor " ^ n)))
	| In(LetADT(name, params, lst), e) ->
		let new_ks, c' = type_adt name params lst c in
		let Type(t1, e1, cs1, ks1) = get_type e c' in
		Type(t1, In(LetADT(name, params, lst), e1), cs1, KindConstraintSet.union new_ks ks1)
	| In(TypeSynonym(name, t), e) ->
		let t, kind, ks = get_kind t c in
		let new_tc = Context.add_kind name kind (Context.add_type_synonym name t c) in
		let Type(t1, e1, cs1, ks1) = get_type e new_tc in
		let new_cs = ConstraintSet.add (Equals(Typevar name, t)) cs1 in
		Type(t1, In(TypeSynonym(name, t), e), new_cs, KindConstraintSet.union ks ks1)
	| Dummy _ | Error _ -> raise(TypeError("Should not appear here"))
	| Match(e, lst) ->
		let Type(t1, e1, cs1, ks1) = get_type e c in
		let res_tv = Ast.new_typevar() in
		let add_list = List.fold_left (fun rest (x, t) -> Context.add_var x t rest) in
		let foldf (p, e) (lst, cs, ks) =
			let rec type_pattern p t cs = match p with
				| PAnything -> (p, [], cs, kem)
				| PVariable x -> (p, [(x, t)], cs, kem)
				| PInt _ -> (p, [], ConstraintSet.add (Equals(t, TInt)) cs, kem)
				| PBool _ -> (p, [], ConstraintSet.add (Equals(t, TBool)) cs, kem)
				| PString _ -> (p, [], ConstraintSet.add (Equals(t, TString)) cs, kem)
				| PPair(p1, p2) ->
					let t1 = Ast.new_typevar() in
					let t2 = Ast.new_typevar() in
					let p1, vars1, cs1, ks1 = type_pattern p1 t1 cs in
					let p2, vars2, cs2, ks2 = type_pattern p2 t2 cs1 in
					let cs = ConstraintSet.add (Equals(t, TProduct(t1, t2))) cs2 in
					(PPair(p1, p2), vars1 @ vars2, cs, KindConstraintSet.union ks1 ks2)
				| PConstructor x ->
					let xt = (try instantiate (Context.find_var x c)
						with Not_found -> raise(TypeError("Unbound constructor " ^ x))) in
					(p, [], ConstraintSet.add (Equals(xt, t)) cs, kem)
				| PApplication(p1, p2) ->
					let t1 = Ast.new_typevar() in
					let t2 = Ast.new_typevar() in
					let p1, vars1, cs1, ks1 = type_pattern p1 t1 cs in
					let p2, vars2, cs2, ks2 = type_pattern p2 t2 cs1 in
					let cs = ConstraintSet.add (Equals(t1, TFunction(t2, t))) cs2 in
					(PApplication(p1, p2), vars1 @ vars2, cs, KindConstraintSet.union ks1 ks2)
				| PGuarded(p, e) ->
					let p, vars, cs, ks = type_pattern p t cs in
					let new_tc = add_list c vars in
					let Type(t', e', cs', ks') = get_type e new_tc in
					let cs = ConstraintSet.add (Equals(t', TBool)) (ConstraintSet.union cs cs') in
					(PGuarded(p, e'), vars, cs, KindConstraintSet.union ks ks')
				| PAs(p, x) ->
					let p, vars, cs, ks = type_pattern p t cs in
					(PAs(p, x), (x, t)::vars, cs, ks)
			in
			let p, vars, cs, ks'' = type_pattern p t1 cs in
			let new_tc = add_list c vars in
			let Type(t', e, cs', ks') = get_type e new_tc in
			let new_cs = ConstraintSet.add (Equals(res_tv, t')) (ConstraintSet.union cs cs') in
			let new_ks = KindConstraintSet.union ks (KindConstraintSet.union ks'' ks') in
			(p, e)::lst, new_cs, new_ks in
		let lst, cs, ks = List.fold_right foldf lst ([], cs1, ks1) in
		Type(res_tv, Match(e1, lst), cs, ks)
	| Module(t, lst) ->
		let rec loop lst c = match lst with
			| [] -> [], [], em, kem
			| (TypeSynonym(name, t) as hd)::tl ->
				let t, kind, ks' = get_kind t c in
				let new_tc = Context.add_kind name kind c in
				let rest, m, cs, ks = loop tl new_tc in
				let new_cs = ConstraintSet.add (Equals(Typevar name, t)) cs in
				ConcreteType(name, [], t)::rest, hd::m, new_cs, (KindConstraintSet.union ks ks')
			| (LetADT(name, params, lst) as hd)::tl ->
				let new_ks, c' = type_adt name params lst c in
				let rest, m, cs, ks = loop tl c' in
				ConcreteType(name, params, TADT lst)::rest, hd::m, cs, (KindConstraintSet.union ks new_ks)
			| Let(x, t, e)::tl ->
				let tc, t, e, cs, ks = get_type_let e x t c in
				let rest, m, cs', ks' = loop tl tc in
				Value(x, t)::rest, Let(x, Some t, e)::m, ConstraintSet.union cs cs', KindConstraintSet.union ks ks'
			| LetRec(x, t, e)::tl ->
				let tc, t, e, cs, ks = get_type_let_rec e x t c in
				let rest, m, cs', ks' = loop tl tc in
				Value(x, t)::rest, LetRec(x, Some t, e)::m, ConstraintSet.union cs cs', KindConstraintSet.union ks ks'
			| SingleExpression e::tl ->
				let Type(_, e, cs1, ks1) = get_type e c in
				let rest, m, cs2, ks2 = loop tl c in
				rest, SingleExpression e::m, ConstraintSet.union cs1 cs2, KindConstraintSet.union ks1 ks2
			| Open m::tl ->
				let rest, m', cs, ks = loop tl (get_type_open m c) in
				rest, Open m::m', cs, ks
			| Import m::tl ->
				let l, tc, cs, ks = get_type_import m c in
				let rest, m', cs', ks' = loop tl tc in
				rest, l::m', ConstraintSet.union cs cs', KindConstraintSet.union ks ks'
		in
		let lst, m, ks, cs = loop lst c in
		(match t with
			| None -> Type(TModule lst, Module(Some(TModule lst), m), ks, cs)
			| Some(TModule lst') ->
				let _ = check_module_type lst' lst in
				Type(TModule lst', Module(t, m), ks, cs)
			| Some _ -> raise(TypeError("Illegal module type")))
	| In(SingleExpression e1, e2) ->
		let Type(t1, e1, cs1, ks1) = get_type e1 c in
		let Type(t2, e2, cs2, ks2) = get_type e2 c in
		Type(t2, Sequence(e1, e2), ConstraintSet.union cs1 cs2, KindConstraintSet.union ks1 ks2)
	| In(Open m, e) ->
		let c' = get_type_open m c in
		let Type(t, e, cs, ks) = get_type e c' in
		Type(t, In(Open m, e), cs, ks)
	| In(Import m, e) ->
		let l, tc, cs, ks = get_type_import m c in
		let Type(t1, e1, cs1, ks1) = get_type e tc in
		Type(t1, In(l, e1), ConstraintSet.union cs cs1, KindConstraintSet.union ks ks1)

and get_type_let (e : expr) (x : string) (t : ltype option) (c : Context.t) : Context.t * ltype * expr * ConstraintSet.t * KindConstraintSet.t =
	let Type(t1, e1, cs1, ks1) = get_type e c in
	let subst = unify cs1 in
	let t1' = apply_substitution subst t1 VariableSet.empty in
	let t1'' = quantify c t1' in
	let t', ks' = get_optional_type t ks1 c in
	let result_t = select_type t' t1'' in
	let new_tc = Context.add_var x result_t c in
	let new_cs = ConstraintSet.add (Equals(t', t1'')) cs1 in
	new_tc, result_t, e1, new_cs, ks'
and get_type_let_rec (e : expr) (x : string) (t : ltype option) (c : Context.t) : Context.t * ltype * expr * ConstraintSet.t * KindConstraintSet.t =
	let t', ks' = get_optional_type t kem c in
	let e1_c = Context.add_var x t' c in
	let Type(t1, e1, cs1, ks1) = get_type e e1_c in
	let subst = unify cs1 in
	let t1' = apply_substitution subst t1 VariableSet.empty in
	let t1'' = quantify e1_c t1' in
	let result_t = select_type t' t1'' in
	let new_tc = Context.add_var x result_t c in
	let ks = KindConstraintSet.union ks' ks1 in
	let new_cs = ConstraintSet.add (Equals(t', t1'')) cs1 in
	new_tc, result_t, e1, new_cs, ks
and get_type_open x (c : Context.t) : Context.t =
	try match Context.find_var x c with
		| TModule lst ->
			let foldf rest elt = match elt with
				| Value(n, t) ->
					Context.add_var n t rest
				| ConcreteType(n, lst, TADT t) ->
					let kc, c' = type_adt n lst t rest in
					c'
				| ConcreteType(_, _, _) ->
					raise(TypeError("Concrete module type must be ADT for now"))
				| AbstractType(n, lst) ->
					let k = List.fold_left (fun rest _ -> KArrow(KStar, rest)) KStar lst in
					Context.add_kind n k rest
			in
			List.fold_left foldf c lst
		| _ as t ->
			raise(TypeError("open expression must be statically resolve as a module type; got " ^ string_of_type t))
	with Not_found -> raise(TypeError("Unbound module: " ^ x))
and get_type_import (m : string) (c : Context.t) =
	let com, new_loc = Util.find_module m (Context.get_loc c) in
	let tc, t, e, cs, ks = get_type_let com m None (Context.set_loc new_loc c) in
	Let(m, Some t, e), tc, cs, ks

let typecheck e verbose loc =
	try let Type(t, e', cs, ks) = get_type e (Context.empty loc) in
		(try
			if verbose then (Printf.printf "Initial type: %s\n" (string_of_type t);
				Printf.printf "Constraints:\n%s\n" (print_cs cs);
				Printf.printf "Kind constraints:\n%s\n" (print_ks ks));
			let subst' = unify_kinds ks in
			let subst = unify cs in
			if verbose then
				let res_t = apply_substitution subst t VariableSet.empty in
				Printf.printf "Kind substitution:\n%s" (print_ksubstitution subst');
				Printf.printf "Substitution:\n%s" (print_substitution subst);
				Printf.printf "Final type: %s\n" (string_of_type res_t)
			else ignore subst;
			Result e'
		with ImpossibleConstraint e -> TError e)
	with TypeError e -> TError e
