
; #t
(print (and))

; #f
(print (and #f))

; #f
(print (and #f (print 3)))

; 3 #t
(print (and #t (print 3)))
