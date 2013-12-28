(define reduce (lambda (f base lst)
    (if (null? lst)
        base
        (f (car lst) (reduce f base (cdr lst))))))

(define filter (lambda (f lst)
    (define foldf (lambda (elt lst)
        (if (f elt)
            (cons elt lst)
            lst)))
    (reduce foldf '() lst)))

(print (filter (lambda (elt) (< elt 3)) '(1 2 3 4)))

(define qsort (lambda (lst)
    (if (null? lst)
        lst
        (let ((pivot (car lst)))
            (define lower (qsort (filter (lambda (elt) (< elt pivot)) (cdr lst))))
            (define upper (qsort (filter (lambda (elt) (>= elt pivot)) (cdr lst))))
            (append lower (cons pivot upper))))))

(print (qsort '(1 3 2 5 4 7 9 6 12 10 11)))
