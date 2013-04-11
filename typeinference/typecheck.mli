type errmsg = string

val typecheck : Ast.expr -> bool -> errmsg option
