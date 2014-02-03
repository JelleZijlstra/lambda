(define x 1)

(define sayit (lambda ()
    (print x)))

(sayit)

(set! x 2)

(sayit)
