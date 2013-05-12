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

val string_of_process : process -> string
val string_of_definition : definition -> string
