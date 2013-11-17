type errmsg = string

type typecheck_t = TError of errmsg | Result of Ast.typed_expr

val typecheck : Ast.typed_expr -> bool -> string -> typecheck_t
