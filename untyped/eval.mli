(** Evaluates an expression according to call-by-value semantics *)
val eval_cbv : Ast.expr -> Ast.value

(** Evaluates an expression according to call-by-name semantics *)
val eval_cbn : Ast.expr -> Ast.value
