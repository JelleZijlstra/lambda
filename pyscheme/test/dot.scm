(print '(3 . (1 2)))

(define testdot (lambda (x . xs) (print x xs)))

(testdot 1 2 3)

(testdot 1)

