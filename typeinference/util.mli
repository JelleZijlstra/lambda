exception FileNotFound of string

val parse_file : string -> Ast.typed_expr

val find_module : string -> string -> Ast.typed_expr * string

val any : ('a -> bool) -> 'a list -> bool
