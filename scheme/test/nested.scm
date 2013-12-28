(define x 1)

(define sayit (lambda ()
    (let ((x 2))
        (print x))))

(sayit)

(set! x 3)

(sayit)
