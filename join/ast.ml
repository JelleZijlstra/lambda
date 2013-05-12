type process =
	PSend of channel * message
	| PCompose of process * process
	| PDef of definition * process
	| PPrint of int
	| PNull
and channel = string
and message = string list
and join_pattern =
	JReceive of channel * message
	| JConjunction of join_pattern * join_pattern
and definition =
	| DDefined of join_pattern * process
	| DConjunction of definition * definition

let join glue =
	List.fold_left (fun a e ->
		let start = if a = "" then "" else a ^ glue in
		start ^ e) ""

let rec string_of_process p = match p with
	| PSend(c, msg) -> c ^ "<" ^ string_of_message msg ^ ">"
	| PCompose(p1, p2) -> string_of_process p1 ^ " | " ^ string_of_process p2
	| PDef(d, p) -> "def " ^ string_of_definition d ^ " in " ^ string_of_process p
	| PPrint n -> "print " ^ string_of_int n
	| PNull -> "()"
and string_of_message msg = join ", " msg
and string_of_definition d = match d with
	| DDefined(jp, p) -> string_of_join_pattern jp ^ " } " ^ string_of_process p
	| DConjunction(d1, d2) -> string_of_definition d1 ^ " ^ " ^ string_of_definition d2
and string_of_join_pattern jp = match jp with
	| JReceive(c, msg) -> c ^ "<" ^ string_of_message msg ^ ">"
	| JConjunction(jp1, jp2) -> string_of_join_pattern jp1 ^ " | " ^ string_of_join_pattern jp2
