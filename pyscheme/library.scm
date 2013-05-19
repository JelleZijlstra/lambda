; standard library for this Scheme dialect, written in Scheme itself

; I don't think actual Scheme accepts this kind of dot-syntax, but it is nice
(define list (lambda (. xs) xs))

(define not (lambda (arg) (if arg #f #t)))

(define apply (lambda (f args) (eval (cons f args))))

(defmacro and (. xs)
	(if (null? xs)
		#t
		(if (not (meval (car xs)))
			#f
			(apply and (cdr xs)))))

(defmacro or (. xs)
	(if (null? xs)
		#f
		(if (meval (car xs))
			#t
			(apply or (cdr xs)))))
