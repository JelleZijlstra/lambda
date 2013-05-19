; standard library for this Scheme dialect, written in Scheme itself

(define list (lambda (x . xs) (cons x xs)))

(define not (lambda (arg) (if arg #f #t)))
