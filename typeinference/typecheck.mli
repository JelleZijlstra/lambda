type errmsg = string

val typecheck : Ast.expr -> errmsg option
