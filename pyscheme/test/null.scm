(define sayit (lambda (lst) (if (null? lst) (print "it's empty") (print "it's not empty"))))

(sayit '())
(sayit '(1 2))
