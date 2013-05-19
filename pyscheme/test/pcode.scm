(defmacro pcode (. xs)
	(if (null? xs)
		'()
		(print (car xs) (apply pcode (cdr xs)))))

(print "no args")
(pcode)

(print "one arg")
(pcode 1)

(print "two args")
(pcode 2 3)
