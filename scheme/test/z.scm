; Recursion using the Z combinator

(define z (lambda (f)
    (let ((g (lambda (x) (f (lambda (v) ((x x) v))))))
        (g g))))

(define length (z (lambda (length) (lambda (lst)
    (if (null? lst)
        0
        (+ 1 (length (cdr lst))))))))

(print (length '(1 2 3)))
(print (length '()))
(print (length '(1)))
