(define apply (lambda (f args) (eval (cons f args))))

(apply print '(3))
